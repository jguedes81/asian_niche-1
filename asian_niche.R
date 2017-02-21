#!/usr/bin/env Rscript
# FedData provides functions for getting GHCN data, 
# and the `pkg_test` function for installing/loading other packages
#install.packages("devtools")
devtools::install_github("bocinsky/FedData")
# Python-style argument parsing
FedData::pkg_test("optparse")

# Set the number of parallel cores
# We'll be using the snow-like functionality with Rmpi
slurm_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))

# Use optparse for Python-style argument parsing
# Define a set of options; only output file and force_redo for now
option_list = list(
  make_option(c("-o", "--output_dir"), type="character", default="./OUTPUT/", 
              help="Output directory [default = %default]", metavar = "character"),
  make_option(c("-n", "--cores"), type="numeric", default=ifelse(is.na(slurm_cores), 2, slurm_cores),
              help="Number of cores for multicore run [default = %default]"),
  make_option(c("-c", "--clean"), action="store_true", default=FALSE,
              help="Delete output directory and re-run all analyses? [default= %default]")
  # make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
  #             help="Print extra output [default= %default]"),
  # make_option(c("-q", "--quietly"), action="store_false",
  #             dest="verbose", help="Print little output [default= %default]"),
  
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

### BEGIN SCRIPT ###

start_time <- Sys.time()
message("asian_niche.R started at ", start_time)

## This is the code for the pan-Asian niche reconstructions

## Load all packages
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

# Packages for chronometric analysis
FedData::pkg_test("Bchron")
FedData::pkg_test("mclust")

# Packages for tidy code
FedData::pkg_test("magrittr")
FedData::pkg_test("hadley/tidyverse")

# Plotting
FedData::pkg_test("RColorBrewer")

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),
                        source)

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

##### SET PARAMETERS #####

# Delete the output directory if requested, then create it
opt$output_dir <- ifelse(stringr::str_sub(opt$output_dir,-1) != "/",
                         stringr::str_c(opt$output_dir,"/"),
                         opt$output_dir) 
if(opt$clean) unlink(opt$output_dir,
                     recursive = TRUE,
                     force = TRUE)
dir.create(opt$output_dir,
           showWarnings = FALSE,
           recursive = TRUE)
# a function that builds output paths
out <- function(...){
  stringr::str_c(opt$output_dir,...)
}

#Define the study region
ASIA_poly <- 
  extent(30,150,5,60) %>% ## THE REAL EURASIAN POLYGON
  # extent(66,72,37,43) %>% ## FOR TESTING PURPOSES ONLY
  FedData::polygon_from_extent("+proj=longlat +ellps=GRS80")

# Set the calibration period for paleoclimate reconstructions
calibration.years <- 1961:1990

# Set this to your google maps elevation api key
# https://developers.google.com/maps/documentation/elevation/start
google_maps_elevation_api_key = "AIzaSyDi4YVDZPt6uH1C1vF8YRpbp1mxqsWbi5M"

# Report the number of cores for parallel processing
message("Number of cores set to SLURM_NTASKS_PER_NODE = ",opt$cores)

##### END SET PARAMETERS #####



##### LOAD ETOPO5 GRID #####

message("Preparing the ETOPO5 grid-aligned dataset")
time_check <-  Sys.time()
if(!opt$clean & file.exists(out("ASIA_rast_etopo5.tif"))){
  ASIA_rast_etopo5 <- raster(out("ASIA_rast_etopo5.tif"))
}else{
  # Get the Natural Earth land data
  dir.create(out("DATA/NaturalEarth/ne_10m_land/"), showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip",destdir=out("DATA/NaturalEarth/"))
  unzip(out("DATA/NaturalEarth/ne_10m_land.zip"), exdir=out("DATA/NaturalEarth/ne_10m_land/"))
  
  dir.create(out("DATA/NaturalEarth/"), showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",destdir=out("DATA/NaturalEarth/"))
  unzip(out("DATA/NaturalEarth/ne_10m_lakes.zip"), exdir=out("DATA/NaturalEarth/ne_10m_lakes/"))
  
  data("ETOPO5")
  ASIA_rast_etopo5 <- ETOPO5 %>%
    t() %>%
    raster(xmn = 0,
           xmx = 360,
           ymn = -90,
           ymx = 90,
           crs = CRS("+proj=longlat +ellps=clrk66 +no_defs")) %>%
    raster::crop(y = sp::spTransform(ASIA_poly, CRSobj = raster::projection(.))) %>%
    raster::mask(sp::spTransform(rgdal::readOGR(out("DATA/NaturalEarth/ne_10m_land/"),"ne_10m_land"),CRSobj = raster::projection(.))) %>%
    raster::mask(sp::spTransform(rgdal::readOGR(out("DATA/NaturalEarth/ne_10m_lakes/"),"ne_10m_lakes"),CRSobj = raster::projection(.)), inverse = TRUE) %T>%
    writeRaster(filename = out("ASIA_rast_etopo5.tif"),
                datatype="INT2S",
                options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND", "PHOTOMETRIC=MINISWHITE"),
                overwrite=T,
                setStatistics=FALSE)
  rm(ETOPO5)
  unlink(out("DATA/NaturalEarth/ne_10m_lakes/"), recursive = T, force = T)
  unlink(out("DATA/NaturalEarth/ne_10m_land/"), recursive = T, force = T)
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
                                force.redo = opt$clean)
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
# Here, we sample from -25 to 25 SD, at 1 SD interval
sample.points <- -25:25

# Read in data on different crop GDD needs
crop_GDD <- readr::read_csv("./DATA/crop_GDD_needs.csv")

# create the cluster for parallel computation
cl <- makeCluster(opt$cores, type = "PSOCK")
registerDoParallel(cl)

# Transform GHCN data to GDDs of each base, and modulate to Marcott
GDDs <- sort(unique(crop_GDD$base_t))
GHCN.GDD.incremented.sd <- foreach::foreach(base = GDDs) %do% {
  
  out.list <- foreach::foreach(change = sample.points,
                               .packages = c("foreach","magrittr")) %dopar% {
                                 
                                 GHCN.GDDs <- foreach::foreach(station = GHCN.data.final$climatology, .combine = c) %do% {
                                   
                                   return(sdModulator(data.df = station,
                                                      temp.change.sd = change,
                                                      t.base = base,
                                                      t.cap = 30))
                                   
                                 }
                                 names(GHCN.GDDs) <- names(GHCN.data.final$climatology)
                                 
                                 return(dplyr::data_frame(SD_change = change,
                                                          ID = names(GHCN.GDDs),
                                                          GDD = GHCN.GDDs))
                                 
                               }
  names(out.list) <- sample.points
  
  return(out.list %>% 
           dplyr::bind_rows() %>%
           dplyr::left_join(GHCN.data.final$spatial %>% 
                              dplyr::as_data_frame() %>% 
                              dplyr::rename(x = LONGITUDE, y = LATITUDE), by = "ID"))
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
  
  return(list(model = model, prediction = prediction))
  
}

# Calculate gdd kriging models for each crop
gdd_model_files <- out("MODELS/",crop_GDD$crop,"_models.rds")

if(opt$clean){
  unlink(out("MODELS"), recursive = TRUE, force = TRUE)
  dir.create(out("MODELS/"), showWarnings = F)
  crop_GDD_run <- crop_GDD
}else{
  dir.create(out("MODELS/"), showWarnings = F)
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
  cl <- makeCluster(min(opt$cores,nrow(crop_GDD_run)), type = "PSOCK")
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
                                     dplyr::do(out_preds = krige_and_predict(.)) %$%
                                     out_preds %>%
                                     sapply("[[","prediction") %>%
                                     apply(1,smooth.preds) %>%
                                     tibble::tibble(model = .) %>%
                                     maptools::spCbind(ASIA_rast_etopo5.sp,.) %>%
                                     readr::write_rds(out("MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"), compress = "xz")
                                   
                                   return(out("MODELS/",crop_GDD_run[crop,"crop"],"_models.rds"))
                                 }
  
  # stop the cluster (will free memory)
  stopCluster(cl)
  
}
message("Calculation indicator Krige models complete: ", capture.output(Sys.time() - time_check))

##### END INTERPOLATE CROP NICHE #####



##### PREDICT CROP NICHE THROUGH TIME #####

## Predicting crop niche from smoothed Krige models
# Calculate niches for each crop using the Marcott et al. 2013.
# create the cluster for parallel computation
message("Generating niche reconstructions")
time_check <-  Sys.time()
if(opt$clean){
  unlink(out("RECONS"), recursive = TRUE, force = TRUE)
}
dir.create(out("RECONS/"), showWarnings = F)

predictor <- function(x, newdata){
  x %>%
    predict(newdata = newdata) %>%
    magrittr::multiply_by(100) %>% 
    round() %>%
    tibble::as_tibble()
}

# cl <- makeCluster(opt$cores, type = "PSOCK")
cl <- makeCluster(min(opt$cores,5), type = "PSOCK")
registerDoParallel(cl)

gdd.recons <- foreach::foreach(crop = crop_GDD$crop,
                               .packages = c("magrittr",
                                             "foreach"),
                               .combine = c) %dopar% {
                                 
                                 if(!file.exists(out("MODELS/",crop,"_models.rds"))) stop("Models for ",
                                                                                          crop,
                                                                                          " are missing! Aborting.")
                                 
                                 crop.recons <- out("RECONS/",crop,"_",c("Z_Lower","Z","Z_Upper"),".nc")
                                 if(all(file.exists(crop.recons))) return(crop.recons)
                                 
                                 crop.models <- readr::read_rds(out("MODELS/",crop,"_models.rds"))
                                 
                                 crop.models@data %<>%
                                   tibble::as_tibble()
                                 
                                 out.files <- foreach::foreach(Zs = c("Z_Lower","Z","Z_Upper"),
                                                               .combine = c) %do% {
                                                                 
                                                                 if(file.exists(out("RECONS/",crop,"_",Zs,".nc"))) 
                                                                   return(out("RECONS/",crop,"_",Zs,".nc"))
                                                                 
                                                                 out_models <- crop.models
                                                                 
                                                                 gc();gc()
                                                                 
                                                                 out_models@data %<>%
                                                                   dplyr::mutate(Zs = lapply(X = model,
                                                                                             FUN = predictor,
                                                                                             newdata = marcott2013[[Zs]])) %$%
                                                                   Zs %>%
                                                                   dplyr::bind_cols() %>%
                                                                   t() %>%
                                                                   tibble::as_tibble() %>%
                                                                   magrittr::set_colnames(marcott2013$YearBP)
                                                                 
                                                                 out_models %>%
                                                                   as("SpatialPixelsDataFrame") %>%
                                                                   raster::brick() %>%
                                                                   raster::setZ(marcott2013$YearBP, name="Years BP") %>%
                                                                   raster::writeRaster(out("RECONS/",crop,"_",Zs,".nc"),
                                                                                       format="CDF",
                                                                                       datatype = "INT1S",
                                                                                       varname="niche_probability",
                                                                                       varunit="unitless", 
                                                                                       longname="Probability of being in the crop niche x 100",
                                                                                       xname="Longitude",
                                                                                       yname="Latitude",
                                                                                       zname="Years BP",
                                                                                       zunit="Years BP",
                                                                                       compression=9,
                                                                                       overwrite=TRUE)
                                                                 
                                                                 rm(out_models)
                                                                 gc();gc()
                                                                 
                                                                 return(out("RECONS/",crop,"_",Zs,".nc"))
                                                                 
                                                               }
                                 return(out.files)
                               }

# stop the cluster (will free memory)
stopCluster(cl)

message("Generation of niche reconstructions complete: ", capture.output(Sys.time() - time_check))

##### END PREDICT CROP NICHE THROUGH TIME #####



##### BEGIN PLOT CROP NICHE THROUGH TIME #####

## Plotting crop niche
# create the cluster for parallel computation
message("Plotting niche reconstructions")
time_check <-  Sys.time()
if(opt$clean){
  unlink(out("PLOTS"), recursive = TRUE, force = TRUE)
}
dir.create(out("PLOTS/"), showWarnings = F)

gdd.recons <- foreach::foreach(n = 1:nrow(crop_GDD),
                               .combine = c) %do% {
                                 
                                 crop <- crop_GDD[n,]$crop
                                 
                                 title <- stringr::str_c(crop_GDD[n,]$Crop_long,
                                                         " — T_base: ",
                                                         crop_GDD[n,]$base_t,
                                                         "°C, Required GDD: ",
                                                         crop_GDD[n,]$min_gdd)
                                 
                                 if(file.exists(out("PLOTS/",crop,".pdf")) & file.exists(out("PLOTS/",crop,".mov")))
                                   return(out("PLOTS/",crop,".pdf"))
                                 
                                 rast <- raster::brick(out("RECONS/",crop,"_Z.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 rast.lower <- raster::brick(out("RECONS/",crop,"_Z_Lower.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 rast.upper <- raster::brick(out("RECONS/",crop,"_Z_Upper.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 years <- rast %>%
                                   names() %>%
                                   gsub(pattern = "X", replacement = "", x = .) %>%
                                   as.numeric()
                                 
                                 pal <- c(rev(colorRampPalette(brewer.pal(9, "Blues")[2:9],
                                                               bias = 1.5,
                                                               space = "Lab")(50)),
                                          colorRampPalette(brewer.pal(9, "Reds")[2:9],
                                                           bias = 1.5,
                                                           space = "Lab")(50))
                                 
                                 if(!file.exists(out("PLOTS/",crop,".pdf")))
                                   space_time_plot(the_brick = rast, 
                                                   the_brick_lower = rast.lower, 
                                                   the_brick_upper = rast.upper, 
                                                   out_file = out("PLOTS/",crop,".pdf"),
                                                   title = title,
                                                   time = years,
                                                   timelim = c(max(years),min(years)),
                                                   timeaxis =  seq(from = max(years)-500,
                                                                   to = min(years),
                                                                   by = -500),
                                                   timelab = "Years BP",
                                                   zbreaks = seq(0,1,0.01),
                                                   zlab = "Probability of being in niche",
                                                   zaxis = seq(0,1,0.1),
                                                   zcolors = pal
                                   )
                                 
                                 if(!file.exists(out("PLOTS/",crop,".mov")))
                                   space_time_video(the_brick = rast, 
                                                    the_brick_lower = rast.lower, 
                                                    the_brick_upper = rast.upper, 
                                                    out_file = out("PLOTS/",crop,".mov"),
                                                    title = title,
                                                    time = years,
                                                    timelim = c(max(years),min(years)),
                                                    timeaxis =  seq(from = max(years)-500,
                                                                    to = min(years),
                                                                    by = -500),
                                                    timelab = "Years BP",
                                                    zbreaks = seq(0,1,0.01),
                                                    zlab = "Probability of being in niche",
                                                    zaxis = seq(0,1,0.1),
                                                    zcolors = pal
                                   )
                                 
                                 return(out("PLOTS/",crop,".pdf"))
                               }

message("Plotting of niche reconstructions complete: ", capture.output(Sys.time() - time_check))

##### END PREDICT CROP NICHE THROUGH TIME #####



##### BEGIN CHRONOMETRIC ANALYSIS #####

## Plotting crop niche
# create the cluster for parallel computation
message("Beginning chronometric co-analysis")

# Read in the cite/chronometric data
chronometric_data <- readr::read_csv("./DATA/chronometric_data.csv", col_types = cols(
  .default = col_logical(),
  `Site identifier` = col_integer(),
  Site = col_character(),
  Longitude = col_double(),
  Latitude = col_double(),
  `Lab sample identifier` = col_character(),
  Material = col_character(),
  `14C age BP` = col_integer(),
  `1-sigma uncertainty` = col_integer(),
  `Age range (BC/AD)` = col_character(),
  `Age range lower (BP)` = col_integer(),
  `Age range upper (BP)` = col_integer(),
  Reference = col_character(),
  `Crops Present` = col_character()
)) %>%
  tibble::as_tibble() %>%
  dplyr::filter(!is.na(Longitude),
                !is.na(Latitude),
                !(is.na(`14C age BP`) & is.na(`Age range lower (BP)`)),
                !is.na(`AMS date on cereal?`))

# Calibrate 14C dates and generate age models for sites
# using Gaussian Mixture density estimation
densities_14C <- chronometric_data %>%
  dplyr::filter(!is.na(`14C age BP`)) %>%
  dplyr::arrange(`Site identifier`) %>%
  dplyr::group_by(`Site identifier`) %>%
  dplyr::do(model = Bchron::BchronDensityFast(ages = .$`14C age BP`,
                                              ageSds = .$`1-sigma uncertainty`,
                                              calCurves = rep('intcal13',nrow(.))))


# A function to generate flat probability density models for sites with only start and end dates
FlatDensity <- function(start_bp,
                        end_bp){
  
  out_model <- function(newdata){
    ifelse(newdata <= start_bp & newdata >= end_bp,
           1 / (start_bp - end_bp),
           0)
  }
  
  class(out_model) = "FlatDensityRun"
  
  return(out_model)
}

# Generate flat probability density models for sites with only start and end dates
densities_other <- chronometric_data %>%
  dplyr::filter(!is.na(`Age range lower (BP)`),
                !is.na(`Age range upper (BP)`))

densities_other %<>%
  dplyr::bind_cols({
    mapply(FUN = FlatDensity, start_bp = densities_other$`Age range lower (BP)`, end_bp = densities_other$`Age range upper (BP)`) %>%
      tibble(model = .)
    }) %>%
  dplyr::select(`Site identifier`, model) %>%
  dplyr::arrange(`Site identifier`)

# Merge the two types
densities <- bind_rows(densities_14C,densities_other) %>%
  dplyr::arrange(`Site identifier`)

# A function to generate predictions from a gaussian mixture model
get_prediction <- function(model, new_x){
  
  if(class(model) == "FlatDensityRun"){
    return({
      model %>%
        do.call(args = list(newdata = new_x)) %>%
        list()
    })
  }
  
  if(class(model) == "BchronDensityRunFast"){
    return({model %$%
        out %>%
        predict(newdata = new_x) %>%
        list()
    })
  }
  
  return("No Model Available")
  
}

# Get the predictions
densities %<>%
  dplyr::bind_cols({
    sapply(densities$model,get_prediction,new_x = marcott2013$YearBP) %>% tibble(density = .)
  })

# # Plot any given site this way
# densities %>%
#   dplyr::filter(`Site identifier` == 10) %$%
#   density %>%
#   magrittr::extract2(1) %>%
#   plot(x = marcott2013$YearBP,
#        y = .,
#        type = 'l',
#        xlim = c(6000,1))

# # or, plot them all with
# plot(1,type = "n",ylim = c(0,0.05), xlim = c(6000,1))
# for(site in unique(densities$`Site identifier`)){
#   densities %>%
#     dplyr::filter(`Site identifier` == site) %$%
#     density %>%
#     magrittr::extract2(1) %>%
#     lines(x = marcott2013$YearBP,
#           y = .,
#           xlab = "Year BP",
#           ylab = "Density")
# }

# # Summing across all distributions yields the net distribution
# densities %$%
#   density %>%
#   do.call(cbind,.) %>%
#   {rowSums(.)/ncol(.)} %>%
#   magrittr::multiply_by(20) %>%
#   plot(x = marcott2013$YearBP,
#        y = .,
#        xlim = c(6000,1),
#        type = "l",
#        xlab = "Year BP",
#        ylab = "Density")


# Create a SpatialPointsDataFrame of the sites
sites <- chronometric_data %>%
  dplyr::select(`Site identifier`,
         Site,
         Longitude,
         Latitude,
         Wheat:Pennesitum) %>%
  dplyr::distinct()

coordinates(sites) <- ~Longitude+Latitude
projection(sites) <- "+proj=longlat"


rast <- raster::brick(out("RECONS/",crop,"_Z.nc")) %>%
  raster:::readAll() %>%
  magrittr::divide_by(100) %>%
  magrittr::extract2(nlayers(.):1)

sites %<>% crop(rast)

sites@data %<>% 
  tibble::as_tibble() %>%
  dplyr::bind_cols({
    rast %>%
      raster::extract(sites) %>%
      t() %>%
      as.data.frame() %>%
      as.list() %>%
      tibble(niche = .)
  })




plot(rast[[1]])
plot(sites, add = T)

test.density <- densities$density[[1]] %>%
  cumsum() %>%
  magrittr::divide_by(sum(densities$density[[1]], na.rm = T))
which(test.density >= 0.05)[1] 
which(test.density >= 0.95)[1]

test.niche <- 20 * densities$density[[1]] * sites@data$niche[[1]]
test.niche %<>%
  cumsum() %>%
  magrittr::divide_by(sum(test.niche, na.rm = T)) 
which(test.niche >= 0.05)[1]
which(test.niche >= 0.95)[1]

sum(20 * densities$density[[1]] * sites@data$niche[[1]], na.rm = T)

##### END CHRONOMETRIC ANALYSIS #####

message("asian_niche.R complete! Total run time: ", capture.output(Sys.time() - start_time))

### END SCRIPT ###