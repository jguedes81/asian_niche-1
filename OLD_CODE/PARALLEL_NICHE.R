#SET THE LIBRARY PATH FOR KAMIAK
.libPaths(c("/data/kamiak/nick.maggio/local/R_Libs","/opt/apps/r/3.2.2/lib64/R/library"))



## SET THE WORKING DIRECTORY TO THE LOCATION WHERE YOU UNZIPPED THIS SOURCE CODE
setwd("/data/kamiak/nick.maggio/JadeDAlpoim/PARALLEL/R")

# Load FedData and rgdal packages.
library(FedData)
library(rgdal)
library(doParallel)

# Mapping and spatial statistics packages
pkg_test("raster")
pkg_test("fields")
pkg_test("geomapdata")
pkg_test("spatstat")
pkg_test("png")
pkg_test("RColorBrewer")
pkg_test("gdata")
pkg_test("minpack.lm")
pkg_test("Hmisc")
pkg_test("zoo")
pkg_test("abind")
pkg_test("plyr")
pkg_test("mgcv")
pkg_test("plotrix")
pkg_test("R.oo")

# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))


# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)
cat("functions loaded\n")

# Read in data on different crop GDD needs
crop_GDD <- read.csv("../DATA/crop_GDD_needs.csv")
cat("crop_GDD_needs.csv read in\n")

# Run the script that extracts and cleans the GHCN weather station data
source("./PREPARE_GHCN.R")
cat("GHCN weather station data read and cleaned\n")

# Read in the cleaned, averaged GHCN data for all stations (including their locations)
GHCN.data.final <- readRDS('../OUTPUT/ghcn_data_final.Rds')

# Split the list into location and climate information
GHCN.stations <- GHCN.data.final[[1]]
GHCN.data <- GHCN.data.final[[2]]
cat("GHCN data stored\n")

# Run the script that transforms the Marcott et al. 2013 data into standard scores.
#source("./PREPARE_MARCOTT.R")

# Run the script that downloads ETOPO5 dataset
data("ETOPO5")

#Keep the orignal ETOPO5 matrix for each parallel computation
ETOPO5_Original <- ETOPO5

# Read in the standard scored data from Marcott et al. 2013
marcott2013 <- read.csv("../OUTPUT/MARCOTT2013_Z.csv")


# Calculate niches for each crop using the Marcott et al. 2013.
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
crops <- list.files("../OUTPUT/SMOOTH", full.names=T) #get all the files in ../OUTPUT/SMOOTH
crops <- crops[grep("ETOPO5", crops)] #remove all files not containing "ETOPO5" in their filename
cat("crop file list captured from SMOOTH directory\n")
foreach(i=1:length(crops)) %dopar% {
#foreach(i=1:1) %dopar% {
  Crop_name <- crops[i]
  Crop_name <- basename(Crop_name) #remove directory structure
  Crop_name <- gsub("_SMOOTHS_ETOPO5","",Crop_name) #get rid of _SMOOTHS_ETOPO5 from cropname
  Crop_name <- gsub(".Rds","",Crop_name) #Remove extension from cropname. NOTE: this leaves the longitudinal limits on the crop name
  ###cat(Crop_name,'\n')}

  #### NEED TO GET LONGITUDINAL LIMITS FROM FILENAME AND REPRODUCE RASTER ETOPO5.rast
  m <- regexpr("[^_]*$", Crop_name, perl=TRUE) #generate regexp object for everything after the last _
  max_lon<- regmatches(Crop_name, m) #capture the string after the last _
  m <- regexpr("_[^_]*$", Crop_name, perl=TRUE) #generate regexp object for everything after and including the last _
  extra <- regmatches(Crop_name, m) #capture the string after and including the last _
  trimmed_name <- gsub(extra,"",Crop_name)
  m <- regexpr("[^_]*$", trimmed_name, perl=TRUE) #generate regexp object for everything after the last _
  min_lon<- regmatches(Crop_name, m) #capture the string after the last _
  cat(Crop_name,'\t',min_lon,'\t',max_lon,'\n')

  
  ## DEFINE EURASIA ##
  #Define the study region
  EURASIA.poly <- polygon_from_extent(extent(as.numeric(min_lon),as.numeric(max_lon),10,60),"+proj=longlat +ellps=GRS80")
  cat("Study Region Defined\n")

  # # Crop the ETOPO5 dataset
  ETOPO5 <- ETOPO5_Original

  # get topo data into x,y,z format
  topo <- matrix(data=ETOPO5,ncol=1)
  lo <- seq(from = 0, to = 359+11/12, by = 1/12);
  la <- seq(from = -89-11/12, to = 90, by = 1/12);
  long = rep(lo,each=length(la))
  lat = rep(la,length(lo))
  # 
  topo <- list()
  topo$x <- lo
  topo$y <- la
  topo$z <- t(apply(ETOPO5,1,rev))
  # 
  # # Eurasia lat/lon
  i.lon <- as.numeric(min_lon)
  x.lon <- as.numeric(max_lon)
  i.lat <- 10 
  x.lat <- 60
  # 
  # # indices for Eurasia
  lon.lim <- which(topo$x>i.lon & topo$x<x.lon)
  lat.lim <- which(topo$y>i.lat & topo$y<x.lat)
  topo$eu <- topo$z[lon.lim,lat.lim]
  #

  # # format for image.plot
  eu.topo <- list()
  eu.topo$x <- topo$x[lon.lim]
  eu.topo$y <- topo$y[lat.lim]
  eu.topo$z <- topo$eu
  # 
  # #transform to raster
  ETOPO5.rast <- raster(eu.topo)
  cat("ETOPO5 raster created\n")


  #if(all(file.exists(paste0("../OUTPUT/MARCOTT/",Crop_name,"_",c("Z_Lower","Z","Z_Upper"),"_ETOPO5.tif")))) next
  crop.data <- readRDS(crops[i])
  for(Zs in c("Z_Lower","Z","Z_Upper")){
    #if(file.exists(paste0("../OUTPUT/MARCOTT/",Crop_name,"_",Zs,"_ETOPO5.tif"))) next
    out <- lapply(1:length(crop.data),function(x){as.integer(predict(crop.data[[x]],newdata=marcott2013[,Zs])*10000)})
    out <- do.call(rbind, out)
    out.rast <- brick(ETOPO5.rast, nl=ncol(out), values=F)
    out.rast[!is.na(ETOPO5.rast[])] <- out
    writeRaster(out.rast,paste0("../OUTPUT/MARCOTT/",Crop_name,"_",Zs,"_ETOPO5.tif"), datatype="INT2U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"),overwrite=T,setStatistics=FALSE)
    print(out.rast)
    rm(out); gc(); gc()
  }
  cat(Crop_name," Finished\n")
}

### END SCRIPT ###
