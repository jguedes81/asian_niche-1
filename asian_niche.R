## This is the code for the pan-Asian niche reconstructions

# install.packages("FedData")
devtools::install_github("bocinsky/FedData")
library(FedData)
pkg_test("parallel")
pkg_test("doParallel")
pkg_test("R.utils")
pkg_test("Hmisc")
pkg_test("zoo")
pkg_test("abind")
pkg_test("mgcv")
pkg_test("rgbif")
pkg_test("rgdal")
pkg_test("ncdf4")
pkg_test("geomapdata")
pkg_test("raster")
pkg_test("readr")
pkg_test("readxl")
pkg_test("magrittr")
pkg_test("plyr")
pkg_test("dplyr")


# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

#Define the study region
# We
ASIA_poly <- polygon_from_extent(extent(30,150,5,60),"+proj=longlat +ellps=GRS80")

# Set the calibration period for paleoclimate reconstructions
calibration.years <- 1961:1990

##### BASIC DATA #####
## The basic data for this analysis includes a 1 arc-min elevation model (ETOPO1)
## that must be limited to landforms.

# Get the Natural Earth land data
dir.create("./OUTPUT/DATA/NaturalEarth/ne_10m_land/", showWarnings = FALSE, recursive = TRUE)
FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip",destdir="./OUTPUT/DATA/NaturalEarth/")
unzip("./OUTPUT/DATA/NaturalEarth/ne_10m_land.zip", exdir="./OUTPUT/DATA/NaturalEarth/ne_10m_land/")

dir.create("./OUTPUT/DATA/NaturalEarth/", showWarnings = FALSE, recursive = TRUE)
FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",destdir="./OUTPUT/DATA/NaturalEarth/")
unzip("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes.zip", exdir="./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/")

# Get the ETOPO1 grid-aligned dataset.
# dir.create("./OUTPUT/DATA/ETOPO1/", showWarnings = FALSE, recursive = TRUE)
# FedData::download_data(url="http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/netcdf/ETOPO1_Bed_g_gdal.grd.gz",destdir="./OUTPUT/DATA/ETOPO1/")
# R.utils::gunzip("./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd.gz", destname="./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd", remove = FALSE)
# ASIA_rast_etopo1 <- raster("./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd") %>%
#   raster::crop(y = sp::spTransform(ASIA_poly, CRSobj = raster::projection(.))) %>%
#   raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_land/","ne_10m_land"),CRSobj = raster::projection(.))) %>%
#   raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/","ne_10m_lakes"),CRSobj = raster::projection(.)), inverse = TRUE) %T>%
#   writeRaster(filename = "./OUTPUT/ASIA_rast_etopo1.tif",
#               datatype="INT2S",
#               options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND", "PHOTOMETRIC=MINISWHITE"),
#               overwrite=T,
#               setStatistics=FALSE)

# Get the ETOPO5 grid-aligned dataset.
data("ETOPO5")
ASIA_rast_etopo5 <- ETOPO5 %>%
  t() %>%
  raster(xmn = 0,
         xmx = 360,
         ymn = -90,
         ymx = 90,
         crs = CRS("+proj=longlat +ellps=clrk66 +no_defs")) %>%
  raster::crop(y = sp::spTransform(ASIA_poly, CRSobj = raster::projection(.))) %>%
  raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_land/","ne_10m_land"),CRSobj = raster::projection(.))) %>%
  raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/","ne_10m_lakes"),CRSobj = raster::projection(.)), inverse = TRUE) %T>%
  writeRaster(filename = "./OUTPUT/ASIA_rast_etopo5.tif",
              datatype="INT2S",
              options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND", "PHOTOMETRIC=MINISWHITE"),
              overwrite=T,
              setStatistics=FALSE)

##### PREPARE THE GHCN DATA #####
## Downloads and cleans daily climate records from the Global Historical Climate Database.

GHCN.data.final <- prepare_ghcn(region = ASIA_poly, 
                                label = "ASIA_poly", 
                                calibration.years = calibration.years, 
                                google_maps_elevation_api_key = "AIzaSyDi4YVDZPt6uH1C1vF8YRpbp1mxqsWbi5M",
                                force.redo = FALSE)

## An example of plotting the GHCN data
# climate_plotter(data = GHCN.data.final, station = "RSM00031474", element = "TMIN")

##### END PREPARE THE GHCN DATA #####

##### PREPARE THE MARCOTT DATA #####
# Run the script that transforms the Marcott et al. 2013 data into standard scores.
marcott2013 <- prepare_marcott(calibration.years = calibration.years)

##### END PREPARE THE MARCOTT DATA #####

##### MODULATING CLIMATOLOGY BY MARCOTT SD #####
#### Calculating growing degree days ####

# How often to sample GDD, in z-space
# Here, we sample from -16 to 10 SD, at 0.1 SD interval
sample.points <- seq(-16,10,0.1)

# A function of correct the indication predictions and estimate a smooth
# monotonic function
# This first uses isotonic regression, then loess smoothing with a degree of 1
smooth.preds <- function(y){
  y[y<0] <- 0
  y[y>1] <- 1
  y <- loess(isoreg(y~sample.points)$yf~sample.points, span=0.05, degree=1)
  return(y)
}

# Read in data on different crop GDD needs
crop_GDD <- read.csv("./DATA/crop_GDD_needs.csv")

# Transform GHCN data to GDDs of each base, and modulate to Marcott
GDDs <- sort(unique(crop_GDD$base_t))
GHCN.GDD.incremented.sd <- lapply(GDDs,function(base){
  out <- lapply(sample.points,function(change){
    GHCN.GDDs <- lapply(GHCN.data.final$climatology,function(station){
      return(sdModulator(data.df=station,
                         temp.change.sd=change,
                         t.base=base))
    })
    return(GHCN.GDDs)
  })
  return(out)
})
names(GHCN.GDD.incremented.sd) <- GDDs

#### Interpolating crop niche extent ####
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# Calculate gdd kriging models for each crop
gdd.models <- lapply(
  1:nrow(crop_GDD),
  FUN=function(crop){
    crop <- crop_GDD[crop,,drop=F]
    # Threshold for indicator kriging
    GHCN.thresh <- lapply(
      GHCN.GDD.incremented.sd[[as.character(crop[['base_t']])]],
      function(year){
        lapply(
          year,
          function(value){
            value>=as.numeric(crop[['min_gdd']])
          })
      })
    
    # Calculate kriges
    fits <- lapply(
      GHCN.thresh,
      function(GHCN.annual.GDDs){
          mKrig(x=coordinates(GHCN.stations),
                y=unlist(GHCN.annual.GDDs),
                Z=GHCN.stations$elevation,
                Covariance="Exponential",
                Distance="rdist.earth")
      })
    
    return(fits)
  })
names(gdd.models) <- crop_GDD$crop
saveRDS(gdd.models,"../OUTPUT/GDD_models_KRIGING.Rds",compress="xz")
gdd.models <- readRDS("../OUTPUT/GDD_models_KRIGING.Rds")



