# Rollback gganimate to 1.x.x to fix bugged final frames
# require(devtools); install_version("gganimate", version = "1.0.7", repos = "http://cran.us.r-project.org")

#### SETUP ####

library(tidyverse)
library(lme4)
library(ggplot2)
library(patchwork)
library(ggeffects)
library(effects)
library(sf)
library(tigris)
library(kriging)
library(basemapR)
library(gganimate)
library(ecodatamisc)
library(ragg)
library(showtext)
library(ggtext)
library(magick)
library(scales)
library(gifski)

# Theme stuff
source("./README assets/theme_ecodata.R")
footer = image_read('./README assets/footer.png') %>% magick::image_background("white") %>% image_ggplot(interpolate=T)
logo="<img src='./README assets/footer.png' height='13' />"

showtext_auto()
showtext_opts(dpi = 300)

#Ecodata
bg = "#1b2724"
wh = "#ffffff"
pl = "#6eb39c"

# data for the per-timestep analyses
dat = read.csv("./Data/Enriched/mastersheet.csv") %>%
  mutate(
    across(c(site_name),as.factor)
  )

pca1 = readRDS('./Data/Objects/pca1.rds')
pca2 = readRDS('./Data/Objects/pca2.rds')

#### Figures ####

# models
m1 = glmer(total_stems ~ last_stems + monitoring_year + yrs_since_trt + amount_lasttrt + treatment_lasttrt + (1|site_name),
           data=dat,family='poisson',nAGQ=0)
m2 = glmer(total_stems ~ last_stems + monitoring_year + PC1_obs + PC1_trt + PC2_obs + PC2_trt + tmax_ann + elev_stddev_m + pct_dist + (1|site_name),
           data=dat,family='poisson',nAGQ=0)


# Figure 1

p1 = ggpredict(m1,terms=c('monitoring_year')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='A',
       x='Monitoring year',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

p2 = ggpredict(m1,terms=c('last_stems')) %>% plot(add.data=F,limit.range=T) +
  geom_abline(intercept=0,slope=1,linetype='dashed',colour='red') +
  scale_x_continuous(limits=c(0,300)) + 
  scale_y_continuous(limits=c(0,300)) +
  labs(subtitle='B',
       x='Stems (t-1)',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

fig1 = (p1|p2)/(plot_spacer()+footer) +
  plot_layout(heights=unit(c(2.7,1), c('in','null')))
ggsave(filename="fig1.png",path="./README assets",width=8,height=4,units='in',dpi=300)


# Figure 2

p1 = ggpredict(m1,terms=c('amount_lasttrt')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='A',
       x='Quantity at last treatment (g)',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

p2 = ggpredict(m1,terms=c('yrs_since_trt')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='B',
       x='Years since last treatment',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

fig2 = (p1|p2)/(plot_spacer()+footer) +
  plot_layout(heights=unit(c(2.7,1), c('in','null')))
ggsave(filename="fig2.png",path="./README assets",width=8,height=4,units='in',dpi=300)


# Figure 3

p1 = ggpredict(m2,terms=c('tmax_ann')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='A',
       x='Annual mean max. temp. (C)',y='Stems (t)') +
  scale_x_continuous(limits=c(12,23)) +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

p2 = ggpredict(m2,terms=c('elev_stddev_m')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='B',
       x='Std. dev of elevation (m)',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

p3 = ggpredict(m2,terms=c('PC1_trt')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='C',
       x='PC1 (treatment)',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )

p4 = ggpredict(m2,terms=c('PC1_obs')) %>% plot(add.data=F,limit.range=T) +
  labs(subtitle='D',
       x='PC2 (treatment)',y='Stems (t)') +
  theme_ecodata_noggtext() +
  theme(
    plot.title = element_blank(),
    axis.line = element_line()
  )


fig3 = (p1|p2)/(p3|p4)/(plot_spacer()+footer) +
  plot_layout(heights=unit(c(2.9,2.9,1), c('in','in','null')))
ggsave(filename="fig3.png",path="./README assets",width=8,height=8,units='in',dpi=300)


# Figure 4 (animation)

co = tigris::states(cb=T,resolution="20m") %>% filter(STUSPS=="CO") %>% st_transform(crs=4326)

sites = dat %>%
  select(site_name,percent_change_from_year_1,monitoring_year,latitude,longitude) %>%
  group_by(site_name,latitude,longitude) %>%
  filter(monitoring_year == max(monitoring_year))

kriged = kriging(
  x=sites$longitude,
  y=sites$latitude,
  response=sites$percent_change_from_year_1
)

kmap = kriged$map %>%
  as.data.frame() %>%
  mutate(i=1) %>%
  full_join(
    data.frame(alpha = seq(0,1,0.1),i=1) %>%
      mutate(t=row_number()),
    by="i",
    relationship = "many-to-many"
  )

an = ggplot(data=kmap) +
  base_map(st_bbox(co),basemap='mapnik',increase_zoom=1,nolabels=T) +
  geom_tile(aes(x=x,y=y,fill=pred,alpha=alpha)) +
  geom_point(data=sites,aes(x=longitude,y=latitude),colour='red') +
  geom_sf(data=co,fill=NA,color='black',linewidth=.5) +
  coord_sf() +
  scale_alpha(guide='none') +
  scale_fill_gradient(low="cyan",high="orange",name="", breaks = c(-1,1,3), labels=c('-100%','100%','300%')) +
  labs(x="",y="",
       title="Change in weed abundance since year 1",
       subtitle="Red points show sites",
       caption=c("Map © 2023 OpenStreetMap",paste0("<br>",logo," "))) +
  scale_x_continuous() + scale_y_continuous() +
  theme_ecodata_noggtext() +
  theme(
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_markdown(size=8,hjust=c(.9,1)),
    legend.position = 'right',
    axis.text.x = element_text(angle=45,hjust=1)
  ) +
  transition_time(t)
an_r = animate(an, fps=20, end_pause = 20, device="ragg_png", width=8, height=6, units="in", res=300)
anim_save("./README assets/krieg.gif", animation=an_r)
