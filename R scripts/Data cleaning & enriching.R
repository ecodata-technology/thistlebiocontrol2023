#### Set up ####

library(tidyverse)
library(sqldf)
library(readxl)
library(janitor)
library(lubridate)
library(daymetr)
library(sf)
library(reticulate)
library(rgee)
library(tigris)
library(nngeo)

climate_window = 13 # days of lookback leading up to treatment application and surveys, we consider 2 weeks (13 days + day of survey)

#### Google earth engine ####

# Set variables for rgee, the google earth engine set-up can be very emotional so refer to repo README for futher instructions
# Make sure filepaths point to python.exe in the ee environment (set-up with ee_install)
# One-time lines of code to set-up google earth engine in R are commented out and flagged as such

Sys.setenv(RETICULATE_MINICONDA_ENABLED=FALSE)
# ee_install() # One-time
# reticulate::py_install('earthengine-api==0.1.323', envname='rgee') # One-time

python_fp = "C:/Users/<USER>/anaconda3/envs/ee/python.exe"
gc_fp = "C:/Users/<USER>/AppData/Local/Google/Cloud SDK/google-cloud-sdk/bin/"
Sys.setenv("RETICULATE_PYTHON" = python_fp)
Sys.setenv("EARTHENGINE_GCLOUD" = gc_fp)

# Do the above, then must terminate R on windows for rgee & reticulate it to recognize any of it
# ee_check() # Check to make sure everything was set up right
# ee_Authenticate() # One-time

google_user = "<username>" # this should be whatever google account you used to sign up for earth engine
ee_Initialize(user=google_user,drive=T,gcs=F)


#### Import and clean data ####

# Monster pipeline to import 
dat = read_xlsx("./Data/Raw/NewCT12mDatabase.xlsx", sheet="12mMonitoringDataMaster") %>%
  clean_names() %>%
  mutate(
    # spot-fix an obvious entry error in Sites 78-79 (incorrect year)
    treatment_date = ifelse(site_name %in% c('Site78','Site79') & monitoring_year == 1, 
                            "9/15/2014",
                            treatment_date),
    most_recent_treatment_date = ifelse(site_name %in% c('Site78','Site79') & monitoring_year == 2,
    # we get dodgy excel date parsing and don't convert until later. 41897 = Sept. 15, 2014.
                            "41897", 
                            most_recent_treatment_date),
    year = year(date_monitored),
    # extracting various different aspects of treatment protocol into more suitable predictors (many go unused in final models)
    targeted = grepl('Targeted',treatment_type),
    broadcast = grepl('Broadcast',treatment_type),
    oil_pct = str_extract(treatment_type,'([0-9]{2})%',group = 1),
    oil = ifelse(is.na(oil_pct),T,F),
    cut_stems = grepl('Cut Stems',treatment_type),
    amount_g = as.numeric( str_extract(treatment_amount,'([0-9]+) g inoculum',group = 1) ),
    amount_lasttrt = amount_g,
    treatment = case_when(targeted & !broadcast ~ 'targeted',
                          !targeted & broadcast ~ 'broadcast',
                          targeted & broadcast ~ 'targeted+broadcast',
                          T ~ NA),
    # set up columns to back-fill with the PRIOR treatment that was applied, new treatments get applied AFTER monitoring in a given year
    treatment_lasttrt = treatment,
    oil_lasttrt = oil,
    # some ugly date parsing due to mixed formats within columns
    trt_year = if_else(treatment_date != "?",
                       year(convert_to_date(treatment_date,string_conversion_failure = "warning")),
                       2019),
    last_trt_year = if_else(most_recent_treatment_date != "?",
                            year(convert_to_date(most_recent_treatment_date,string_conversion_failure = "warning")),
                            2019),
    treatment_date = if_else(grepl('[0-9]+',treatment_date),
                             convert_to_date(treatment_date,string_conversion_failure = "warning"),
                             NA_Date_),
    yrs_since_trt = if_else(is.na(last_trt_year),
                            0,
                            year(date_monitored) - last_trt_year),
    # set up arguments for extracting climate data from a window of time prior to treatment/surveys
    window_start = treatment_date-climate_window,
    obs_start = date_monitored-climate_window,
    # spot-fix a couple of bad longitude entries  
    longitude = ifelse(longitude > 0, -1*longitude, longitude)
  ) %>%
  group_by(site_name) %>%
  # for each observation fill in the values applied at the prior treatment
  fill(amount_lasttrt) %>% fill(treatment_lasttrt) %>% fill(oil_lasttrt) %>%
  mutate(
    # get stem counts at t-1 and the change from t-1 to t
    last_stems = lag(total_stems),
    delta_stems = (total_stems/last_stems)-1,
    # make sure we have date format not posixct as the latter causes problems with SQL queries later on
    treatment_date = as.Date(treatment_date),
    date_monitored = as.Date(date_monitored),
    window_start = as.Date(window_start),
    obs_start = as.Date(obs_start)
  ) %>%
  ungroup()


#### Enrich data ####
#### Climate

# store a list of sites and GPS coordinates for scraping covariates
sites = dat %>%
  select(site_name,latitude,longitude) %>%
  distinct()

# store first and final years in the timeseries for daymet scraping
yrmin = min(dat$year)-1; yrmax = max(dat$year)

# for each site, for each treatment date, get the average tmin, tmax, precip, vp, rad
dmt = sites %>%
  # daymet functions need long/lat only in the input
  select(latitude,longitude) %>%
  mutate(cl = pmap(., .f = function(latitude, longitude){
    download_daymet(lat = latitude, lon = longitude, start=yrmin, end=yrmax, internal=T, force=F) %>%
      .$data %>%
      rename(
        tmax = `tmax..deg.c.`,
        tmin = `tmin..deg.c.`,
        daylen = `dayl..s.`,
        prcp = `prcp..mm.day.`,
        srad = `srad..W.m.2.`,
        vp = `vp..Pa.`,
        swe = `swe..kg.m.2.`
      )
  }
  )) %>%
  # reattach site names
  right_join(sites,by=c("latitude","longitude")) %>%
  unnest(cl) %>%
  # calculate average temp and get julian days for each year
  mutate(
    tavg = (tmin+tmax)/2,
    date = as.Date(paste(yday,year), '%j %Y')
  )

# annualize daily max temperature
ann_averages = dmt %>%
  group_by(site_name,year) %>%
  summarise(tmax_ann = mean(tmax))

# fetch average climate values over the n day window leading up to treatment, for each treatment event at each site
# we do this with a sql query as it can handle joins over variable date ranges in a way dplyr can't
averages = sqldf("
  SELECT s.site_name, s.treatment_date,
    avg(cl.tmax) AS tmax_trt,
    avg(cl.tmin) AS tmin_trt,
    avg(cl.tavg) AS tavg_trt,
    avg(cl.prcp) AS prcp_trt,
    avg(cl.vp) AS vp_trt,
    avg(cl.srad) AS srad_trt
  FROM dat as s
  LEFT JOIN dmt AS cl
    ON s.site_name=cl.site_name AND cl.date >= s.window_start AND cl.date <= s.treatment_date
  GROUP BY s.site_name, s.treatment_date
                        ") %>%
  filter(!is.na(treatment_date)) %>%
  mutate(
    treatment_year = year(treatment_date)
  )

# as above, but for the window leading up to surveys
obs_averages = sqldf("
  SELECT s.site_name, s.date_monitored,
    avg(cl.tmax) AS tmax_obs,
    avg(cl.tmin) AS tmin_obs,
    avg(cl.tavg) AS tavg_obs,
    avg(cl.prcp) AS prcp_obs,
    avg(cl.vp) AS vp_obs,
    avg(cl.srad) AS srad_obs
  FROM dat as s
  LEFT JOIN dmt AS cl
    ON s.site_name=cl.site_name AND cl.date >= s.obs_start AND cl.date <= s.date_monitored
  GROUP BY s.site_name, s.date_monitored
                        ") %>%
  filter(!is.na(date_monitored))


#### Google earth engine
# here we scrape an elevation model and land use data

#### land usage from ESA v2
# define the areas to measure around each site
cover = sites %>%
  # as land use data is relatively patchy we'll just consider land use in the first year of the study (by site)
  left_join(dat %>% group_by(site_name) %>% summarise(yr1 = min(year)), by=c("site_name")) %>%
  # create an sf with crs set to wgs84
  st_as_sf(coords=c("longitude","latitude"), crs = st_crs(4326)) %>%
  # then convert to Albers for accurate areas
  st_transform(crs=5070) %>%
  # create a 1km radius buffer around each point (this is where we will calculate land use)
  st_buffer(dist=1000) %>%
  # convert back to wgs84
  st_transform(crs=4326)
# check to see the points and buffers look right
# leaflet::leaflet(data=cover) %>% leaflet::addTiles() %>% leaflet::addPolygons()

# get a map of colorado to constrain the earth engine call later and save a lot of unnecessary data being downloaded
co = tigris::states(cb=T,resolution="20m") %>% filter(STUSPS=="CO") %>%
  st_transform(crs=4326) %>%
  sf_as_ee()

# request and store the dataset in an object, commented out after initial download as it's very slow

# wc = ee$ImageCollection("ESA/WorldCover/v200") %>%
#   ee$ImageCollection$map(function(x) x$select("Map")) %>%
#   ee$ImageCollection$toBands() # from imagecollection to image
# 
# ee_as_raster(
#     image=wc,
#     region=cov_ee$geometry(),
#     # raise maxpixels to avoid an error cos the map is so big
#     maxPixels=1e+10,
#     scale=20,
#     dsn = "./Data/Objects/ESA_landuse.tif"
#   )

# # inspect the layer
# Map$addLayer(eeObject=wc)
# ESPG 3857

# import the freshly exported raster
co_raster = raster::raster('./Data/Objects/ESA_landuse.tif')

# extract the pixel values, which are mapped to different land uses here: https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200#bands
all_pix = raster::extract(co_raster,cover)

# it comes in nested lists, so calculating per-site is really gross
# for each site
for(i in 1:length(cover$site_name)) {
  # store how many pixels there are (for % later)
  cover$len[i] = length(all_pix[[i]])
  # get counts of pixels in each land-use category
  cover$raw_dist[i] = table(unlist(lapply(all_pix[[i]], unique))) %>% 
    as.data.frame() %>%
    # count the pixels specifically in categories 40 and 50 (cropland and built-up)
    filter(Var1 %in% c(40,50)) %>%
    select(Freq) %>%
    sum()
  # calculate percent cropland/built-up around each site
  cover$pct_dist[i] = cover$raw_dist[i] / cover$len[i] #calculate % disturbed
}


#### 'flatness'
# define buffer as for land use, but bigger relative to the distances we eyeballed from sites to nearby airports (2.5km radius)
buffer2_5 = sites %>%
  left_join(dat %>% group_by(site_name) %>% summarise(yr1 = min(year)), by=c("site_name")) %>%
  st_as_sf(coords=c("longitude","latitude"), crs = st_crs(4326)) %>%
  st_transform(crs=5070) %>%
  st_buffer(dist=2500) %>%
  st_transform(crs=4326)

# use USGS elevation model with 10m grain
ev = ee$Image("USGS/3DEP/10m")

# extract using the mask to get standard deviation of elevation per site
flatness = ee_extract(x = ev, y = buffer2_5["site_name"],
                      scale=10.2, sf = T,
                      # use fun arg to calculate std dev for each polygon specified by y (i.e., by site)
                      fun = ee$Reducer$stdDev()) %>% 
  rename(elev_stddev_m = elevation)

#### Distance to nearest airport (m)
# not available in EE, sourced from https://data.colorado.gov/Transportation/Airports-in-Colorado/x5bw-ax3d/data?no_mobile=true
aps = read_csv('./Data/Enriched/AIRPORTS.csv') %>%
  st_as_sf(coords=c("LONG_","LAT"), crs = st_crs(4326))

# be a little lazy and directly bind distance to airport to the landuse dataframe
cover = cover %>%
  bind_cols(
    # use nngeo for easy distance to nearest neighbour
    ap_nn_m = (nngeo::st_nn(cover,aps,k=1, returnDist=T))$dist %>%
      unlist()
  )

#### Merging, derived predictors
# attach all the environmental data to mastersheet, for treatment data fill down so each row has the climate data from the most recent treatment date
dat2 = left_join(dat,averages,by=c("site_name" = "site_name","last_trt_year" = "treatment_year")) %>%
  left_join(obs_averages,by=c("site_name" = "site_name","date_monitored" = "date_monitored")) %>%
  left_join(ann_averages,by=c("site_name" = "site_name","year" = "year")) %>%
  mutate(
    # calculate relative humitidy and dewpoint over treatment and observation/survey windows
    # calculate RH % from vp (Pa) and es (kPa), equation: https://glossary.ametsoc.org/wiki/Clausius-clapeyron_equation
    # dewpoint from https://journals.ametsoc.org/view/journals/bams/86/2/bams-86-2-225.xml?tab_body=pdf
    RH_trt = vp_trt/(1000*.6112*exp( (17.67*tavg_trt)/(tavg_trt + 243.5) )), 
    dp_trt = tavg_trt - (100-100*RH_trt)/5, 
    RH_obs = vp_obs/(1000*.6112*exp( (17.67*tavg_obs)/(tavg_obs + 243.5) )),
    dp_obs = tavg_obs - (100-100*RH_obs)/5
  ) %>%
  left_join(cover %>% select(site_name,pct_dist,ap_nn_m), by='site_name') %>%
  left_join(flatness %>% select(site_name,elev_stddev_m), by='site_name')


#### PCA ####
# PCA inputs a bit picky to prefilter to what we want to include
pca_dat = dat2 %>%
  select(site_name,year,elevation_m,tmax_trt:srad_obs,RH_trt:dp_obs) %>%
  drop_na() %>%
  mutate(site_year = paste(site_name,year))

# try pca on combined treatment and observation window data
pca0 = prcomp(pca_dat %>% select(-site_name,-year,-site_year),center=T,scale=T)
summary(pca0)
# pc1 explains 44.1%, pc2 is 17.2%
# OK but not great

# since treatment and observation windows are a few months apart on average there's likely some seasonal differences causing issues when pooling, so pca them separately
pca1 = prcomp(pca_dat %>% select(elevation_m:srad_trt,RH_trt,dp_trt),center=T,scale=T)
summary(pca1)

pca_dat2 = dat2 %>%
  select(site_name,year,elevation_m,tmax_obs:srad_obs,RH_obs,dp_obs) %>%
  drop_na()
pca2 = prcomp(pca_dat2 %>% select(elevation_m,tmax_obs:srad_obs,RH_obs:dp_obs),center=T,scale=T)
summary(pca2)
# for separate treatment and observation pcas, we get similar and much better % (54-55, 21-28) on first two PCs

# visualise loadings
biplot(pca1)
biplot(pca2)

# store the pcas for using in reports
write_rds(pca1, './Data/Objects/pca1.rds')
write_rds(pca2, './Data/Objects/pca2.rds')

# to get the scores for each row, bind 'x' in pca1 and 2 onto the data df
dat3 = dat2 %>%
  left_join(
    bind_cols(pca_dat,pca1$x %>% as.data.frame() %>% select(PC1,PC2)) %>%
      rename(PC1_trt = PC1, PC2_trt = PC2) %>%
      select(site_name,year,PC1_trt,PC2_trt),
    by=c('site_name','year') 
  ) %>%
  left_join(
    bind_cols(pca_dat2,pca2$x %>% as.data.frame() %>% select(PC1,PC2)) %>%
      rename(PC1_obs = PC1, PC2_obs = PC2) %>%
      select(site_name,year,PC1_obs,PC2_obs),
    by=c('site_name','year')
  )
    
# export the enriched data mastersheet  
write_csv(dat3,"./Data/Enriched/mastersheet.csv")
