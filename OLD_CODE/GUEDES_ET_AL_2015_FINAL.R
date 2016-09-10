## SET THE WORKING DIRECTORY TO THE LOCATION WHERE YOU UNZIPPED THIS SOURCE CODE
# setwd("~/directory/of/source/R/")

# Download the FedData package, and load it.
# THIS CODE WILL ONLY WORK WITH VERSION 1.1.0 OF FedData
install.packages("FedData")
library(FedData)
# Mapping and spatial statistics packages
pkgTest("raster")
pkgTest("fields")
pkgTest("geomapdata")
pkgTest("spatstat")
pkgTest("png")
pkgTest("RColorBrewer")
pkgTest("gdata")
pkgTest("minpack.lm")
pkgTest("Hmisc")
pkgTest("zoo")
pkgTest("abind")
pkgTest("plyr")
pkgTest("mgcv")
pkgTest("plotrix")
pkgTest("R.oo")

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)









# Read in data on different crop GDD needs
crop_GDD <- read.csv("../DATA/crop_GDD_needs.csv")

# Run the script that extracts and cleans the GHCN weather station data
source("./PREPARE_GHCN.R")

# Read in the cleaned, averaged GHCN data for all stations (including their locations)
GHCN.data.final <- readRDS('../OUTPUT/ghcn_data_final.Rds')
# Split the list into location and climate information
GHCN.stations <- GHCN.data.final[[1]]
GHCN.data <- GHCN.data.final[[2]]

# Run the script that transforms the Marcott et al. 2013 data into standard scores.
source("./PREPARE_MARCOTT.R")

# Read in the standard scored data from Marcott et al. 2013
marcott2013 <- read.csv("../OUTPUT/MARCOTT2013_Z.csv")

## DEFINE THE TIBETAN PLATEAU ##
# Define the study region
TIBET.poly <- polygonFromExtent(extent(75,105,25,41),"+proj=longlat +ellps=GRS80")

# Run the script that downloads and crops the ETOPO1 dataset
source("./PREPARE_ETOPO.R")


#### Modulating climate by standard deviation ####
#### Calculating growing degree days ####

# How often to sample GDD, in z-space
# Here, we sample from -15 to 15 SD, at 0.1 SD interval
sample.points <- seq(-15,15,0.1)

# A function of correct the indication predictions and estimate a smooth
# monotonic function
# This first uses isotonic regression, then loess smoothing with a degree of 1
smooth.preds <- function(y){
  y[y<0] <- 0
  y[y>1] <- 1
  y <- loess(isoreg(y~sample.points)$yf~sample.points, span=0.05, degree=1)
  return(y)
}

# Transform GHCN data to GDDs of each base, and modulate to Marcott
GDDs <- sort(unique(crop_GDD$base_t))
GHCN.GDD.incremented.sd <- lapply(GDDs,function(base){
  out <- lapply(sample.points,function(change){
    GHCN.GDDs <- lapply(GHCN.data,function(station){
      return(sdModulator(data.df=station,temp.change.sd=change,t.base=base))
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
# gdd.models <- lapply(1:nrow(crop_GDD),FUN=function(crop){
#   crop <- crop_GDD[crop,,drop=F]
#   # Threshold for indicator kriging
#   GHCN.thresh <- lapply(GHCN.GDD.incremented.sd[[as.character(crop[['base_t']])]],function(year){lapply(year,function(value){value>=as.numeric(crop[['min_gdd']])})})
#   
#   # Calculate kriges
#   fits <- lapply(GHCN.thresh, function(GHCN.annual.GDDs){
#     return(mKrig(x=coordinates(GHCN.stations),y=unlist(GHCN.annual.GDDs),Z=GHCN.stations$elevation,Covariance="Exponential", Distance="rdist.earth"))
#   })
#   
#   return(fits)
# })
# names(gdd.models) <- crop_GDD$crop
# saveRDS(gdd.models,"../OUTPUT/GDD_models_KRIGING.Rds",compress="xz")
gdd.models <- readRDS("../OUTPUT/GDD_models_KRIGING.Rds")

## Predicting grop niche from Krige models
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# # Read the prepared Tibetan Plateau ETOPO1 raster
# ETOPO1.rast <- raster("../OUTPUT/TIBETAN_PLATEAU_ETOPO1.tif")
# 
# # Extract the raster coordinates for Kriging
# ETOPO1.rast.coords <- coordinates(ETOPO1.rast)[!is.na(ETOPO1.rast[]),]
# ETOPO1.rast.values <- ETOPO1.rast[!is.na(ETOPO1.rast[])]
# 
# # function to generate predictions
# predict.rast <- function(fits){
#   test.smooths <-  mapply(FUN=function(x,y,Z){
#     test <- sapply(fits,function(fit){
#       predict.mKrig(fit,matrix(c(x,y),ncol=2), Z=Z)
#     })
#     return(smooth.preds(test))
#   },x=ETOPO1.rast.coords[,'x'],y=ETOPO1.rast.coords[,'y'],Z=ETOPO1.rast.values, SIMPLIFY=F)
#   
#   return(test.smooths)
# }
# 
# for(fits in names(gdd.models)){
#   system.time(smooths <- predict.rast(gdd.models[[fits]]))
#   saveRDS(smooths,paste0("../OUTPUT/SMOOTH/",fits,"_SMOOTHS_ETOPO1.Rds"),compress="xz")
#   rm(smooths)
#   gc();gc()
# }
# 
# # Cleanup unneeded data
# rm(ETOPO1.rast.coords); rm(ETOPO1.rast.values); gc(); gc()



# Calculate niches for each crop using the Marcott et al. 2013.
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# crops <- list.files("../OUTPUT/SMOOTH", full.names=T)
# crops <- crops[grep("ETOPO1", crops)]
# for(Crop in crops){
#   Crop.name <- basename(Crop)
#   Crop.name <- gsub("_SMOOTHS_ETOPO1.Rds","",Crop.name)
#   if(all(file.exists(paste0("../OUTPUT/MARCOTT/",Crop.name,"_",c("Z_Lower","Z","Z_Upper"),"_ETOPO1.tif")))) next
#   crop.data <- readRDS(Crop)
#   for(Zs in c("Z_Lower","Z","Z_Upper")){
#     if(file.exists(paste0("../OUTPUT/MARCOTT/",Crop.name,"_",Zs,"_ETOPO1.tif"))) next
#     out <- lapply(1:length(crop.data),function(x){as.integer(predict(crop.data[[x]],newdata=marcott2013[,Zs])*10000)})
#     out <- do.call(rbind, out)
#     out.rast <- brick(TIBET, nl=ncol(out), values=F)
#     out.rast[!is.na(TIBET[])] <- out
#     writeRaster(out.rast,paste0("../OUTPUT/MARCOTT/",Crop.name,"_",Zs,"_ETOPO1.tif"), datatype="INT2U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"),overwrite=T,setStatistics=FALSE)
#     print(out.rast)
#   }
# }

### END SCRIPT ###