## This is the code for the pan-Asian niche reconstructions

# install.packages("FedData")
library(FedData)
pkg_test("parallel")
pkg_test("doParallel")
pkg_test("R.utils")
pkg_test("rgdal")
pkg_test("ncdf4")
pkg_test("geomapdata")
pkg_test("raster")
pkg_test("magrittr")

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
dir.create("./OUTPUT/DATA/ETOPO1/", showWarnings = FALSE, recursive = TRUE)
FedData::download_data(url="http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/netcdf/ETOPO1_Bed_g_gdal.grd.gz",destdir="./OUTPUT/DATA/ETOPO1/")
R.utils::gunzip("./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd.gz", destname="./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd", remove = FALSE)
ASIA_rast_etopo1 <- raster("./OUTPUT/DATA/ETOPO1/ETOPO1_Bed_g_gdal.grd") %>%
  raster::crop(y = sp::spTransform(ASIA_poly, CRSobj = raster::projection(.))) %>%
  raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_land/","ne_10m_land"),CRSobj = raster::projection(.))) %>%
  raster::mask(sp::spTransform(rgdal::readOGR("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/","ne_10m_lakes"),CRSobj = raster::projection(.)), inverse = TRUE) %T>%
  writeRaster(filename = "./OUTPUT/ASIA_rast_etopo1.tif",
              datatype="INT2S",
              options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND", "PHOTOMETRIC=MINISWHITE"),
              overwrite=T,
              setStatistics=FALSE)

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

library(gdalUtils)

# Set the window increment, width and height
incr <- 10
win_width <- 100
win_height <- 100

# Create a data.frame containing coordinates of the lower-left
#  corners of the windows, and the corresponding output filenames.
xy <- setNames(expand.grid(seq(0, dims[1], incr), seq(dims[2], 0, -incr)), 
               c('llx', 'lly'))
xy$nm <- paste0(xy$llx, '-', dims[2] - xy$lly, '.png')

# Create a function to split the raster using gdalUtils::gdal_translate
split_rast <- function(infile, outfile, llx, lly, win_width, win_height) {
  gdal_translate(infile, outfile, 
                 srcwin=c(llx, lly - win_height, win_width, win_height))
}

library(parallel)
cl <- makeCluster(4) # e.g. use 4 cores
clusterExport(cl, c('split_rast', 'xy')) 

system.time({
  parLapply(cl, seq_len(nrow(xy)), function(i) {
    split_rast('R1fqE.jpg', xy$nm[i], xy$llx[i], xy$lly[i], 100, 100)  
  })
})
stopCluster(cl)

##### PREPARE THE GHCN DATA #####
## Downloads and cleans daily climate records from the Global Historical Climate Database.


# Run the script that downloads ETOPO5 dataset
data("ETOPO5")

# Read in data on different crop GDD needs
crop_GDD <- read.csv("./DATA/crop_GDD_needs.csv")
