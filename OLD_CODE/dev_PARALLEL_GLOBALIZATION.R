#SET THE LIBRARY PATH FOR KAMIAK
.libPaths(c("/data/kamiak/nick.maggio/local/R_Libs","/opt/apps/r/3.2.2/lib64/R/library"))



## SET THE WORKING DIRECTORY TO THE LOCATION WHERE YOU UNZIPPED THIS SOURCE CODE
setwd("/data/kamiak/nick.maggio/JadeDAlpoim/cleanUpPost/R")

# Load FedData, rgdal and doParallel packages.
library(FedData)
library(rgdal)
library(doParallel)

# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
# cl <- makeCluster(Sys.getenv("SLURM_NTASKS_PER_NODE"))
# registerDoParallel(cl)

registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

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

# Read in the standard scored data from Marcott et al. 2013
marcott2013 <- read.csv("../OUTPUT/MARCOTT2013_Z.csv")
cat("Marcott data transformed into standard scores and stored\n")

# Read in interpolation models from Guedes, Manning, and Bocinsky 2015
gdd.models <- readRDS("../OUTPUT/GDD_models_KRIGING.Rds")
cat("interpolation models read in, names:\n")


# Setup Parallel run variables in one of two ways 
# First method, running over a contiguous geographical region
initial_lon     <- 30  #Western most longitude
final_lon       <- 150 #Eastern most longitude
dx              <- 5   #spatial stepsize in degrees longitude
num_iterations  <- (final_lon-dx-initial_lon)/dx

# Second method, specifying a discrete set of longitudinal windows
# vec_lon         = c(55,60,75,115) #vector of initial longitudes
# dx              <- 5   #spatial stepsize in degrees longitude
# num_iterations  <- length(vec_lon)-1

############################################################################
##################### BEGIN PARALLEL REGION ################################
############################################################################
foreach(i=0:num_iterations) %dopar% {
  if(exists("initial_lon")){
    min_lon<-initial_lon+i*dx
  }else{
    min_lon <- vec_lon[i+1]
  }
  max_lon<-min_lon+dx
  cat('min_lon: ',min_lon,'    max_lon: ',max_lon,'\n')

  #NICK MAGGIO: Defining output file suffix for saving conventions
  output_suffix <- paste0("_SMOOTHS_ETOPO5_",toString(min_lon),"_",toString(max_lon),".Rds")

  #Define the study region
  EURASIA.poly <- polygon_from_extent(extent(min_lon,max_lon,10,60),"+proj=longlat +ellps=GRS80")
  cat("Study Region Defined\n")

  # # Run the script that downloads and crops the ETOPO5 dataset
  data("ETOPO5")
  cat("dataset downloaded\n")

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
  i.lon <- min_lon
  x.lon <- max_lon
  i.lat <- 10 
  x.lat <- 60
  # 
  # # indices for Eurasia
  lon.lim <- which(topo$x>i.lon & topo$x<x.lon)
  lat.lim <- which(topo$y>i.lat & topo$y<x.lat)
  topo$eu <- topo$z[lon.lim,lat.lim]
  #
  cat("topo data defined and transformed\n")

  # # format for image.plot
  eu.topo <- list()
  eu.topo$x <- topo$x[lon.lim]
  eu.topo$y <- topo$y[lat.lim]
  eu.topo$z <- topo$eu
  # 
  # #transform to raster
  ETOPO5.rast <- raster(eu.topo)
  # 
  # 
  projection(ETOPO5.rast) <- projection(EURASIA.poly)
  # 
  plot(ETOPO5.rast)
  cat("plotting complete\n")




  # # Extract the raster coordinates for Kriging
  ETOPO5.rast.coords <- coordinates(ETOPO5.rast)[!is.na(ETOPO5.rast[]),]
  ETOPO5.rast.values <- ETOPO5.rast[!is.na(ETOPO5.rast[])]
  cat("raster coordinates extracted for Kriging\n")

  dir.create("../OUTPUT/SMOOTH", showWarnings = F, recursive = T)
  dir.create("../OUTPUT/MARCOTT", showWarnings = F, recursive = T)

  # How often to sample GDD, in z-space
  # Here, we sample from -15 to 15 SD, at 0.1 SD interval
  sample.points <- seq(-15,15,0.1)

  ############################################################################
  ##################### BEGIN INLINE FUNCTIONS ###############################
  ############################################################################

  # A function of correct the indication predictions and estimate a smooth
  # monotonic function
  # This first uses isotonic regression, then loess smoothing with a degree of 1
  smooth.preds <- function(y){
    y[y<0] <- 0
    y[y>1] <- 1
    y <- loess(isoreg(y~sample.points)$yf~sample.points, span=0.05, degree=1)
    return(y)
  }


  # function to generate predictions
  predict.rast <- function(fits){
    test.smooths <-  mapply(FUN=function(x,y,Z){
      test <- sapply(fits,function(fit){
        predict.mKrig(fit,matrix(c(x,y),ncol=2), Z=Z)
      })
      return(smooth.preds(test))
    },x=ETOPO5.rast.coords[,'x'],y=ETOPO5.rast.coords[,'y'],Z=ETOPO5.rast.values, SIMPLIFY=F)
    
    return(test.smooths)
  }

  ############################################################################
  ####################### END INLINE FUNCTIONS ###############################
  ############################################################################


  cat("begining fits loop\n")
  for(fits in names(gdd.models)){
    if(fits=="Barley_Alaska"){
    	system.time(smooths <- predict.rast(gdd.models[[fits]]))
    	saveRDS(smooths,paste0("../OUTPUT/SMOOTH/",fits,output_suffix),compress="xz")
    	rm(smooths)
    	gc();gc()
    }
  }
  cat("\nfits loop complete\n")

  # Cleanup unneeded data
  rm(ETOPO5.rast.coords); rm(ETOPO5.rast.values); gc(); gc()
  cat("unneeded data cleanup completed\n")
}
############################################################################
####################### END PARALLEL REGION ################################
############################################################################
### END SCRIPT ###
