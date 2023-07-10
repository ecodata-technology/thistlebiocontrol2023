# Utah Canada Thistle Project

This repository contains the scripts and data for the Utah Canada Thistle analyses performed for Schaeffer lab by Ecodata. Canada Thistle (*Cirsium arvense*) is a widespread perennial weed - the dataset contains the results of biocontrol trials using a fungal rust *Puccinia punctiformis*. 

![](https://github.com/ecodata-technology/utah_schaffer_annotated/blob/main/README%20assets/thistle_publicdomain.jpg)
Image credits - National Park Gallery/Keller, 1977

For this project we cleaned and restructured the raw data to work around its limitations, enriched it with climate and geographic data scraped from DAYMET and Google Earth Engine, and conducted analyses using linear mixed effects models, principal components analyses, and ordinary krieging.

## Key Files

- **R scripts/Data cleaning & enriching.R**: This script imports, cleans, and enriches the data with climate variables and land use and topographic data from Google Earth Engine. It also calculates distance to the nearest airport (which was ultimately rejected by linear mixed models) and a Principal Component Analysis (PCA) on climate data, which enabled us to work around the sparsity of the dataset to explore how climate impacted pest populations. Site names have been anonymized in the raw data.

- **Reports/Methods and Results.RMD**: This Rmarkdown file contains an outline of the methods we used and the results of the analyses. In summary, we used Poisson generalized linear mixed models, ordinary krieging, and principal component analyses (PCA).

![Figure 1](https://github.com/ecodata-technology/utah_schaffer_annotated/blob/main/README%20assets/fig1.png) 

Figure 1: Overall we found A. a general year-on-year decline in *C. arvense* stem counts, with B. evidence of negative density dependence.

![Figure 2](https://github.com/ecodata-technology/utah_schaffer_annotated/blob/main/README%20assets/fig2.png)

Figure 2: Though biocontrol performance relative to controls was unclear, we show that management outcomes are likely better when A. *P. punctiformis* inoculum is applied in higher quantity, and B. when inoculum is applied more frequently.

![Figure 3](https://github.com/ecodata-technology/utah_schaffer_annotated/blob/main/README%20assets/fig3.png)

Figure 3: The climate model-geographic model showed that A. lower stem counts were associated with hotter years, and B. rougher terrain, which may help to guide targeted management. Higher stem counts were also associated with higher values of PC1 at time of treatment application (higher elevation, higher radiance; and lower temperatures, vapour pressure, and humidity), and lower values of PC1 around sampling (lower radiance and elevation; and higher temperatures, dew point, vapour pressure, humidity, and precipitation).

![Figure 4](https://github.com/ecodata-technology/utah_schaffer_annotated/blob/main/README%20assets/krieg.gif)

Figure 4: Change in *C. arvense* stem counts from beginning to end of the timeseries was spatially autocorrelated across Colorado based on Moran's I and ordinary krieging.

## Setting up Google Earth Engine in R

The process can be tricky. GEE runs on Python and is executed in R via the reticulate and rgee packages, relying on authentication with Google Cloud and Drive. Specific configuration is required to make sure they work together.

1. Register for a non-commercial Earth Engine account [here](https://signup.earthengine.google.com/). It may take a few days to get approved.

2. Install [Anaconda](https://www.anaconda.com/products/distribution) and make sure to add it to your PATH during installation.

3. Install [Google Cloud SDK](https://cloud.google.com/sdk/docs/install), including the bundled Python build.

4. After installation, you need to authenticate with a Google account.

5. Install the `rgee`, `reticulate`, and `googledrive` libraries in R. You can find more information about `rgee` [here](https://github.com/r-spatial/rgee).

6. In Windows, you MUST run RStudio as an administrator.

7. Run the following commands in RStudio, replacing the paths with your own user name to point to the appropriate files/folders (Windows installation assumed, see rgee github for Linux and Mac):

```r
Sys.setenv(RETICULATE_MINICONDA_ENABLED=FALSE)
Sys.setenv("RETICULATE_PYTHON" = "C:/Users/yourusername/anaconda3/python.exe")
Sys.setenv("EARTHENGINE_GCLOUD" = "C:/Users/yourusername/AppData/Local/Google/Cloud SDK/google-cloud-sdk/bin/")
```
8. In Windows you then must terminate R.

9. Run `rgee::ee_install()` in RStudio to build the Python environment that will contain Earth Engine.

10. Run `ee_check()` to make sure it's all installed correctly.

11. Install the correct version of Earth Engine. You need to install version 0.1.329 or `rgee` breaks. This is easier to do within R:

```r
reticulate::py_install('earthengine-api==0.1.329', envname='rgee')
```

12. Terminate R again.

13. Finally, initialize Earth Engine with the following commands:

```r
ee_Initialize(user="yourusername",drive=T,gcs=F)
ee_Authenticate()
```

Replace "yourusername" with your Google username that you used for Earth Engine.

14. Also, go to Tools > Global Options > Python > Conda > Select rgee environment in RStudio to avoid library errors in future sessions.
