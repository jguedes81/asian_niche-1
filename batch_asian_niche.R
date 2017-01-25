## This is the code for the pan-Asian niche reconstructions
setwd("/data/cas/bocinsky/git/asian_niche/")

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

# Packages for tidy code
FedData::pkg_test("magrittr")
FedData::pkg_test("hadley/tidyverse")

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

#Define the study region
ASIA_poly <- FedData::polygon_from_extent(extent(30,150,5,60),"+proj=longlat +ellps=GRS80")

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
}else{
  cores = slurm_cores
}

##### BASIC DATA #####
# Get the ETOPO5 grid-aligned dataset.
if(force.redo | !file.exists("./OUTPUT/ASIA_rast_etopo5.tif")){
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
} else {
  ASIA_rast_etopo5 <- raster("./OUTPUT/ASIA_rast_etopo5.tif")
}

##### PREPARE THE GHCN DATA #####
## Downloads and cleans daily climate records from the Global Historical Climate Database.

GHCN.data.final <- prepare_ghcn(region = ASIA_poly, 
                                label = "ASIA_poly", 
                                calibration.years = calibration.years, 
                                google_maps_elevation_api_key = google_maps_elevation_api_key,
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


#### Interpolating crop niche extent ####
# Create a spatialPointsDataFrame of the etopo5 data, and convert to WGS84 ellipsoid
ASIA_rast_etopo5 <- raster("./OUTPUT/ASIA_rast_etopo5.tif") # %>%
#  crop(extent(90,92,40,42))
ASIA_rast_etopo5.sp <- ASIA_rast_etopo5 %>% raster::rasterToPoints(spatial = T) %>%
  sp::spTransform(sp::CRS(raster::projection(GHCN.data.final$spatial)))
ASIA_rast_etopo5.temp <- ASIA_rast_etopo5

# A function that predicts SE from krige model
predictSE.mKrig <- function (object, xnew = NULL, Z = NULL, verbose = FALSE, drop.Z = FALSE, 
                             ...) 
{
  call.name <- object$cov.function.name
  if (is.null(xnew)) {
    xnew <- object$x
  }
  # if ((!drop.Z) & !is.null(object$Z)) {
  #   Z <- object$Z
  # }
  xnew <- as.matrix(xnew)
  if (!is.null(Z)) {
    Z <- as.matrix(Z)
  }
  if (verbose) {
    print(xnew)
    print(Z)
  }
  lambda <- object$lambda
  rho <- object$rhohat
  sigma2 <- lambda * rho
  if (verbose) {
    print(c(lambda, rho, sigma2))
  }
  k0 <- do.call(call.name, c(object$args, list(x1 = object$x, 
                                               x2 = xnew)))
  if (!drop.Z) {
    t0 <- t(cbind(fields.mkpoly(xnew, m = object$m), Z))
  }
  else {
    stop(" drop.Z not supported")
  }
  hold <- mKrig.coef(object, y = k0)
  temp1 <- rho * (colSums(t0 * (object$Omega %*% t0)) - colSums((k0) * 
                                                                  hold$c) - 2 * colSums(t0 * hold$d))
  temp0 <- rho * do.call(call.name, c(object$args, list(x1 = xnew, 
                                                        marginal = TRUE)))
  temp <- temp0 + temp1
  return(sqrt(temp))
}

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
                                                 Z = .$ASIA_rast_etopo5) %>%
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

# A function of correct the indication predictions and estimate a smooth
# monotonic function
# This first uses isotonic regression, then loess smoothing with a degree of 1
smooth.preds <- function(y){
  y[y<0] <- 0
  y[y>1] <- 1
  y <- loess(isoreg(y~sample.points)$yf~sample.points, span=0.1, degree=1)
  return(y)
}

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
                                   readr::write_rds(paste0("./OUTPUT/MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"), compress = "xz")
                                 
                                 return(paste0("./OUTPUT/MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"))
                               }

# stop the cluster (will free memory)
stopCluster(cl)

## Predicting crop niche from smoothed Krige models
# Calculate niches for each crop using the Marcott et al. 2013.
# create the cluster for parallel computation
if(force.redo){
  unlink("./OUTPUT/RECONS", recursive = TRUE, force = TRUE)
}
dir.create("./OUTPUT/RECONS/", showWarnings = F)

cl <- makeCluster(min(cores,nrow(crop_GDD)), type = "PSOCK")
registerDoParallel(cl)

gdd.recons <- foreach::foreach(crop = crop_GDD$crop,
                               .packages = c("dplyr","magrittr","foreach","doParallel","readr","ncdf4","raster"),
                               .export = c("sample.points")) %dopar% {
                                 
                                 if(all(file.exists(paste0("./OUTPUT/RECONS/",crop,"_",c("Z_Lower","Z","Z_Upper"),".tif")))) return(NULL)
                                 
                                 if(!file.exists(paste0("./OUTPUT/MODELS/",crop,"_models.rds"))) return(NULL)
                                 
                                 crop.models <- readr::read_rds(paste0("./OUTPUT/MODELS/",crop,"_models.rds"))
                                 
                                 foreach::foreach(Zs = c("Z_Lower","Z","Z_Upper")) %do% {
                                   
                                   if(file.exists(paste0("./OUTPUT/RECONS/",crop,"_",Zs,".tif"))) return(NULL)
                                   
                                   crop.predictions <- foreach::foreach(crop.model = crop.models,
                                                                        .combine = rbind) %do% {
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