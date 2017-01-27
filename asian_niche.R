start_time <- Sys.time()
message("asian_niche.R started at ", start_time)

## This is the code for the pan-Asian niche reconstructions
# Set the working directory, either from parameters passed to Rscript, or by default
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0){
  setwd(as.character(args[1]))
}

message("Working directory set to ",getwd())

## Load all packages
# FedData provides functions for getting GHCN data, 
# and the `pkg_test` function for installing/loading other packages
devtools::install_github("bocinsky/FedData")

# Packages for parallel processeing
FedData::pkg_test("foreach")
FedData::pkg_test("doParallel")

# Packages offering general utilities
FedData::pkg_test("R.utils")
FedData::pkg_test("Hmisc")
FedData::pkg_test("zoo")
FedData::pkg_test("abind")
FedData::pkg_test("mgcv")
FedData::pkg_test("rgbif")
FedData::pkg_test("fields")

# Packages for spatial processing
FedData::pkg_test("rgdal")
FedData::pkg_test("ncdf4")
FedData::pkg_test("raster")
FedData::pkg_test("geomapdata")
FedData::pkg_test("maptools")

# Packages for tidy code
FedData::pkg_test("magrittr")
FedData::pkg_test("hadley/tidyverse")


##### SET PARAMETERS #####

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

#Define the study region
ASIA_poly <- 
  # extent(30,150,5,60) %>% ## THE REAL EURASIAN POLYGON
  extent(66,72,37,43) %>% ## FOR TESTING PURPOSES ONLY
  FedData::polygon_from_extent("+proj=longlat +ellps=GRS80")

# Set the calibration period for paleoclimate reconstructions
calibration.years <- 1961:1990

# Redo all calculations?
force.redo = FALSE

# Set this to your google maps elevation api key
google_maps_elevation_api_key = "AIzaSyDi4YVDZPt6uH1C1vF8YRpbp1mxqsWbi5M"

# Set the number of parallel cores
# We'll be using the snow-like functionality with Rmpi
slurm_cores <- Sys.getenv("SLURM_NTASKS_PER_NODE") %>% as.numeric()
if (is.na(slurm_cores)){
  cores = 2
  message("SLURM_NTASKS_PER_NODE not available! Number of cores set to ",cores)
}else{
  cores = slurm_cores
  message("Number of cores set to SLURM_NTASKS_PER_NODE = ",cores)
}

##### END SET PARAMETERS #####



##### LOAD ETOPO5 GRID #####

message("Preparing the ETOPO5 grid-aligned dataset")
time_check <-  Sys.time()
if(!force.redo & file.exists("./OUTPUT/ASIA_rast_etopo5.tif")){
  ASIA_rast_etopo5 <- raster("./OUTPUT/ASIA_rast_etopo5.tif")
}else{
  # Get the Natural Earth land data
  dir.create("./OUTPUT/DATA/NaturalEarth/ne_10m_land/", showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip",destdir="./OUTPUT/DATA/NaturalEarth/")
  unzip("./OUTPUT/DATA/NaturalEarth/ne_10m_land.zip", exdir="./OUTPUT/DATA/NaturalEarth/ne_10m_land/")
  
  dir.create("./OUTPUT/DATA/NaturalEarth/", showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",destdir="./OUTPUT/DATA/NaturalEarth/")
  unzip("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes.zip", exdir="./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/")
  
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
  rm(ETOPO5)
  unlink("./OUTPUT/DATA/NaturalEarth/ne_10m_lakes/", recursive = T, force = T)
  unlink("./OUTPUT/DATA/NaturalEarth/ne_10m_land/", recursive = T, force = T)
}

message("ETOPO5 grid-aligned dataset preparation complete: ", capture.output(Sys.time() - time_check))

##### END LOAD ETOPO5 GRID #####



##### PREPARE THE GHCN DATA #####

## Downloads and cleans daily climate records from the Global Historical Climate Database.
message("Preparing the daily climate records from the Global Historical Climate Database")
time_check <-  Sys.time()
GHCN.data.final <- prepare_ghcn(region = ASIA_poly, 
                                label = "ASIA_poly", 
                                calibration.years = calibration.years, 
                                google_maps_elevation_api_key = google_maps_elevation_api_key,
                                force.redo = force.redo)
message("GHCN preparation complete: ", capture.output(Sys.time() - time_check))
## An example of plotting the GHCN data
# climate_plotter(data = GHCN.data.final, station = "CHM00051334", element = "TMIN")

##### END PREPARE THE GHCN DATA #####



##### PREPARE THE MARCOTT DATA #####

# Run the script that transforms the Marcott et al. 2013 data into standard scores.
message("Preparing the Marcott et al. 2013 data")
time_check <-  Sys.time()
marcott2013 <- prepare_marcott(calibration.years = calibration.years)
message("Marcott et al. 2013 data preparation complete: ", capture.output(Sys.time() - time_check))

##### END PREPARE THE MARCOTT DATA #####



##### MODULATING CLIMATOLOGY BY MARCOTT SD #####

#### Calculating growing degree days ####
message("Modulating local climatology by Marcott et al. 2013 data")
time_check <-  Sys.time()
# How often to sample GDD, in z-space, for model tuning
# Here, we sample from -20 to 20 SD, at 1 SD interval (this is probably overkill)
sample.points <- -20:20

# Read in data on different crop GDD needs
crop_GDD <- readr::read_csv("./DATA/crop_GDD_needs.csv")

# create the cluster for parallel computation
cl <- makeCluster(cores, type = "PSOCK")
registerDoParallel(cl)

# Transform GHCN data to GDDs of each base, and modulate to Marcott
GDDs <- sort(unique(crop_GDD$base_t))
GHCN.GDD.incremented.sd <- foreach::foreach(base = GDDs) %do% {
  
  out <- foreach::foreach(change = sample.points,
                          .packages = c("foreach","magrittr")) %dopar% {
                            
                            GHCN.GDDs <- foreach::foreach(station = GHCN.data.final$climatology, .combine = c) %do% {
                              
                              sdModulator(data.df = station,
                                          temp.change.sd = change,
                                          t.base = base,
                                          t.cap = 30) %>%
                                return()
                              
                            }
                            names(GHCN.GDDs) <- names(GHCN.data.final$climatology)
                            
                            dplyr::data_frame(SD_change = change,
                                              ID = names(GHCN.GDDs),
                                              GDD = GHCN.GDDs) %>%
                              return()
                            
                          }
  names(out) <- sample.points
  
  out %>% 
    dplyr::bind_rows() %>%
    dplyr::left_join(GHCN.data.final$spatial %>% 
                       dplyr::as_data_frame() %>% 
                       dplyr::rename(x = LONGITUDE, y = LATITUDE), 
                     by = "ID") %>%
    return()
}
names(GHCN.GDD.incremented.sd) <- GDDs

# stop the cluster (will free memory)
stopCluster(cl)
message("Modulation of local climatology by Marcott et al. 2013 data complete: ", capture.output(Sys.time() - time_check))

##### END MODULATING CLIMATOLOGY BY MARCOTT SD #####



##### INTERPOLATE CROP NICHE #####

message("Calculating indicator Krige models")
time_check <-  Sys.time()
# Create a spatialPointsDataFrame of the etopo5 data, and convert to WGS84 ellipsoid
ASIA_rast_etopo5.sp <- 
  ASIA_rast_etopo5 %>%
  magrittr::set_names("elevation") %>%
  raster::rasterToPoints(spatial = T) %>%
  sp::spTransform(sp::CRS(raster::projection(GHCN.data.final$spatial)))

# A function that generates the kriging model, then predicts
krige_and_predict <- function(dt){
  model <- fields::mKrig(x = dt[,c("x","y")],
                         y = dt$GDD_thresh,
                         Z = dt$elevation,
                         Covariance = "Exponential",
                         Distance = "rdist.earth")
  
  prediction <- ASIA_rast_etopo5.sp %>%
    tibble::as_tibble() %>%
    dplyr::mutate(chunk = rep(1:ceiling(nrow(.)/chunk_size),length.out = nrow(.)) %>%
                    sort()
    ) %>%
    dplyr::group_by(chunk) %>%
    dplyr::do(prediction = fields::predict.mKrig(model,
                                                 xnew = .[,c("x","y")],
                                                 Z = .$elevation) %>%
                as.vector()) %$%
    prediction %>%
    unlist()
  
  list(model = model, prediction = prediction) %>%
    return()
  
}

# Calculate gdd kriging models for each crop
gdd_model_files <- paste0("./OUTPUT/MODELS/",crop_GDD$crop,"_models.rds")

if(force.redo){
  unlink("./OUTPUT/MODELS", recursive = TRUE, force = TRUE)
  dir.create("./OUTPUT/MODELS/", showWarnings = F)
  crop_GDD_run <- crop_GDD
}else{
  dir.create("./OUTPUT/MODELS/", showWarnings = F)
  crop_GDD_run <- crop_GDD[!file.exists(gdd_model_files),]
}

if(nrow(crop_GDD_run) == 0){
message("All indicator Krige models have already been calculated. Continuing.")
}else message("Calculating indicator Krige models for ",
              nrow(crop_GDD_run),
              " cultivars:\n",
              paste0(capture.output(crop_GDD_run), collapse = "\n"))


# A function of correct the indication predictions and estimate a smooth
# monotonic function
# This first uses isotonic regression, then loess smoothing with a degree of 1
smooth.preds <- function(y){
  y[y<0] <- 0
  y[y>1] <- 1
  y <- loess(isoreg(y~sample.points)$yf~sample.points, span=0.1, degree=1)
  return(y)
}

if(nrow(crop_GDD_run) > 0){
  # create the cluster for parallel computation
  cl <- makeCluster(min(cores,nrow(crop_GDD_run)), type = "PSOCK")
  registerDoParallel(cl)
  
  options(dplyr.show_progress = FALSE)
  chunk_size <- 10000
  
  gdd.models <- foreach::foreach(crop = 1:nrow(crop_GDD_run),
                                 .packages = c("fields","dplyr","magrittr","foreach","doParallel","readr"),
                                 .export = c("sample.points")) %dopar% {
                                   
                                   # Threshold for indicator kriging
                                   GHCN.GDD.incremented.sd[[as.character(crop_GDD_run[crop,"base_t"])]] %>%
                                     dplyr::mutate(GDD_thresh = {GDD >= as.numeric(crop_GDD_run[crop,"min_gdd"])}) %>%
                                     dplyr::group_by(SD_change) %>%
                                     dplyr::do(out = krige_and_predict(.)) %$%
                                     out %>%
                                     sapply("[[","prediction") %>%
                                     apply(1,smooth.preds) %>%
                                     tibble::tibble(model = .) %>%
                                     maptools::spCbind(ASIA_rast_etopo5.sp,.) %>%
                                     readr::write_rds(paste0("./OUTPUT/MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"), compress = "xz")
                                   
                                   return(paste0("./OUTPUT/MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"))
                                 }
  
  # stop the cluster (will free memory)
  stopCluster(cl)
  
}
message("Calculation indicator Krige models complete: ", capture.output(Sys.time() - time_check))

##### END INTERPOLATE CROP NICHE #####
test <- readr::read_rds(paste0("./OUTPUT/MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"))
test@data %<>%
  tibble::as_tibble() %>%
  dplyr::mutate(Z = lapply(X = model,
                             FUN = predict,
                             newdata = marcott2013[["Z"]]))
# 
# test %>%
#   as("SpatialPixelsDataFrame") %>%
#   raster::raster(layer = "Z0") %>%
#   plot()
#   spplot("Z0")
# 
# 
# %$%
#   `Z0` %>%
#   range()
#   
# test@data$model[[1]] %>%
#   predict(newdata = -10:10)






## Predicting crop niche from smoothed Krige models
# Calculate niches for each crop using the Marcott et al. 2013.
# create the cluster for parallel computation
if(force.redo){
  unlink("./OUTPUT/RECONS", recursive = TRUE, force = TRUE)
}
dir.create("./OUTPUT/RECONS/", showWarnings = F)

cl <- makeCluster(cores, type = "PSOCK")
# cl <- makeCluster(4, type = "PSOCK")
registerDoParallel(cl)

gdd.recons <- foreach::foreach(crop = crop_GDD$crop) %do% {
                                 
                                 if(!file.exists(paste0("./OUTPUT/MODELS/",crop,"_models.rds"))) stop("Models for ",
                                                                                                      crop,
                                                                                                      " are missing! Aborting.")
                                 
                                 crop.models <- readr::read_rds(paste0("./OUTPUT/MODELS/",crop,"_models.rds"))
                                 
                                 crop.models@data %<>%
                                   tibble::as_tibble() %>%
                                   dplyr::mutate(Z_Lower = lapply(X = model,
                                                            FUN = predict,
                                                            newdata = marcott2013[["Z_Lower"]]),
                                                 Z = lapply(X = model,
                                                            FUN = predict,
                                                            newdata = marcott2013[["Z"]]),
                                                 Z_Upper = lapply(X = model,
                                                            FUN = predict,
                                                            newdata = marcott2013[["Z_Upper"]])
                                                 )
                                 
                                 
                                 foreach::foreach(Zs = c("Z_Lower","Z","Z_Upper")) %do% {
                                   
                                   if(file.exists(paste0("./OUTPUT/RECONS/",crop,"_",Zs,".tif"))) return(NULL)
                                   
                                   crop.predictions <- foreach::foreach(crop.model = crop.models,
                                                                        .combine = rbind,
                                                                        .packages = c("dplyr","magrittr","foreach","doParallel","readr","ncdf4","raster"),
                                                                        .export = c("sample.points")) %dopar% {
                                                                          predict(crop.model, newdata = marcott2013[[Zs]]) %>%
                                                                            magrittr::multiply_by(10000) %>% 
                                                                            as.integer()
                                                                        }
                                   
                                   crop.predictions.rast <- brick(ASIA_rast_etopo5, nl=ncol(crop.predictions), values=F)
                                   crop.predictions.rast[!is.na(ASIA_rast_etopo5[])] <- crop.predictions
                                   writeRaster(crop.predictions.rast,
                                               paste0("./OUTPUT/RECONS/",crop,"_",Zs,".tif"),
                                               datatype = "INT2U",
                                               options = c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"),
                                               overwrite = T,
                                               setStatistics = FALSE)
                                   
                                   return(paste0("../OUTPUT/RECONS/",crop,"_",Zs,".tif"))
                                 }
                                 
                               }
# stop the cluster (will free memory)
stopCluster(cl)
### END SCRIPT ###