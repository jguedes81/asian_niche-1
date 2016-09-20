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
pkg_test("geosphere")
pkg_test("matrixStats")
pkg_test("raster")
pkg_test("magrittr")
pkg_test("plyr")
pkg_test("hadley/tidyverse")
pkg_test("e1071")

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

#Define the study region
# We
ASIA_poly <- polygon_from_extent(extent(30,150,5,60),"+proj=longlat +ellps=GRS80")

# Set the calibration period for paleoclimate reconstructions
calibration.years <- 1961:1990

# Redo all calculations?
force.redo = FALSE

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
ASIA_rast_etopo1 <- raster("./OUTPUT/ASIA_rast_etopo1.tif")

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
                                force.redo = force.redo)

## An example of plotting the GHCN data
# climate_plotter(data = GHCN.data.final, station = "CHM00051334", element = "TMIN")

##### END PREPARE THE GHCN DATA #####

##### PREPARE THE MARCOTT DATA #####
# Run the script that transforms the Marcott et al. 2013 data into standard scores.
marcott2013 <- prepare_marcott(calibration.years = calibration.years)

##### END PREPARE THE MARCOTT DATA #####

##### MODULATING CLIMATOLOGY BY MARCOTT SD #####
#### Calculating growing degree days ####

# How often to sample GDD, in z-space
# Here, we sample from -20 to 20 SD, at 1 SD interval
sample.points <- -20:20

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
                         t.base=base,
                         t.cap=30))
    })
    return(data_frame(SD_change = change, ID = names(GHCN.GDDs), GDD = unlist(GHCN.GDDs)))
  })
  return({
    out %>% 
      dplyr::bind_rows() %>%
      dplyr::left_join(GHCN.data.final$spatial %>% as_data_frame() %>% rename(x = coords.x1, y = coords.x2), by = "ID")
  })
})
names(GHCN.GDD.incremented.sd) <- GDDs

##### MODEL THE NICHE PROBABILITY USING SVM #####
# Calculate gdd svm models for each base GDD value

# Get the distance matrix between stations, using the geodetic on the WGS84 ellipsoid
GHCN.data.final.distm <- geosphere::distm(GHCN.data.final$spatial, fun = distGeo)
GHCN.data.final.distm[GHCN.data.final.distm == 0] <- NA
GHCN.data.final.distm.mean <- GHCN.data.final.distm %>% matrixStats::rowMins(na.rm = T) %>% mean() # mean distance to nearest neighboring station is 111 km

GHCN.GDD.incremented.sd_0_ecef <- wgs84_to_ecef(lon = GHCN.GDD.incremented.sd$`0`$x, 
                                                lat = GHCN.GDD.incremented.sd$`0`$y, 
                                                elev = GHCN.GDD.incremented.sd$`0`$elevation)
GHCN.GDD.incremented.sd$`0` %<>%
  dplyr::mutate(x_ecef = GHCN.GDD.incremented.sd_0_ecef$'x',
                y_ecef = GHCN.GDD.incremented.sd_0_ecef$'y',
                z_ecef = GHCN.GDD.incremented.sd_0_ecef$'z')

dist(GHCN.GDD.incremented.sd_0_ecef %>% as_data_frame())

test.model <- svm(as.factor(GDD >= 2000) ~ x_ecef + y_ecef + z_ecef + elevation + SD_change, data = GHCN.GDD.incremented.sd$`0`, probability = T)

ASIA_rast_etopo5.points <- rasterToPoints(ASIA_rast_etopo5) %>% as_data_frame()
ASIA_rast_etopo5.ecef <- wgs84_to_ecef(lon = ASIA_rast_etopo5.points$x, lat = ASIA_rast_etopo5.points$y, elev = ASIA_rast_etopo5.points$layer) %>% as_data_frame()

# ASIA_rast_etopo5_0 <- ASIA_rast_etopo5
test.out <- lapply(-20:20,function(SD_change){
  ASIA_rast_etopo5_0[!is.na(ASIA_rast_etopo5_0)] <- predict(test.model, 
                                                            newdata = 
                                                              ASIA_rast_etopo5.ecef %>% 
                                                              rename(x_ecef = x,
                                                                     y_ecef = y,
                                                                     z_ecef = z) %>%
                                                              dplyr::mutate(elevation = (ASIA_rast_etopo5.points$layer),
                                                                            SD_change = SD_change),
                                                            probability = T
  ) %>% attr(which = "probabilities") %>% as_data_frame() %>% .[["TRUE"]]
  return(ASIA_rast_etopo5_0)
})
test.out %<>% brick()
names(test.out) <- paste0("SD: ",as.character(-20:20))
plot(y = test.out[14400], x =-20:20)
plot(test.out[[1]], zlim = c(0,1))

GHCN.GDD.incremented.sd$`0` %>%
  filter(ID == "TX000038750") %>%
  plot(GDD~SD_change, data = .)
abline(a = 4173.36467, b=1500)

GHCN.GDD.incremented.sd$`0` %>%
  filter(SD_change == 0) %>%
  arrange(-elevation)


if(force.redo | !file.exists("./OUTPUT/gdd_krige_models.Rds"))
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
saveRDS(gdd.models,"./OUTPUT/gdd_krige_models.Rds",compress="xz")
gdd.models <- readRDS("./OUTPUT/gdd_krige_models.Rds")


ASIA_rast_etopo5_0 <- ASIA_rast_etopo5
test.rast <- ASIA_rast_etopo5_0 %>% raster::crop(extent(85,95,25,35))

# install.packages("plotly")
library("plotly")

plot_ly(test.out, x = x, y = y, z = z, type = "scatter3d", color = test$layer)
plot_ly(z = test.out, type = "surface")
