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

##### END PREPARE THE GHCN DATA #####

# Read in data on different crop GDD needs
crop_GDD <- read.csv("./DATA/crop_GDD_needs.csv")
