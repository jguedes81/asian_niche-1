#!/usr/bin/env Rscript

# FedData provides functions for getting GHCN data, 
# and the `pkg_test` function for installing/loading other packages
#install.packages("devtools")
# devtools::install_cran("FedData")
# Python-style argument parsing
FedData::pkg_test("optparse")

# Set the number of parallel cores
# We'll be using the snow-like functionality with Rmpi
slurm_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))

# Use optparse for Python-style argument parsing
# Define a set of options; only output file and force_redo for now
option_list = list(
  make_option(c("-o", "--output_dir"),
              type = "character",
              default = "./OUTPUT/", 
              help = "Output directory [default = %default]",
              metavar = "character"),
  make_option(c("-n", "--cores"),
              type = "numeric",
              default = ifelse(is.na(slurm_cores), 2, slurm_cores),
              help = "Number of cores for multicore run [default = %default]"),
  make_option(c("-c", "--clean"),
              action = "store_true",
              default = FALSE,
              help = "Delete output directory and re-run all analyses? [default= %default]")
); 

opt_parser = OptionParser(option_list = option_list);
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
FedData::pkg_test("sf")
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
FedData::pkg_test("plotly")
FedData::pkg_test("htmlwidgets")

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

# Create a directory for writing tables
c("DATA","MODELS","RECONS","FIGURES","TABLES","SITE_DENSITIES") %>%
  purrr::walk(~ dir.create(out(.),
                           showWarnings = FALSE,
                           recursive = TRUE))

# A function to set an objects class
set_class <- function(x, classes){
  class(x) <- classes
  return(x)
}

crop_custom <- function(x, y) {
  x.sp <- as(x, "Spatial")
  y.sp <- as(y, "Spatial")
  x.sp.crop <- raster::crop(x.sp, y.sp)
  st_as_sf(x.sp.crop)
}

#Define the study region
ASIA_poly <- 
  extent(55,125,5,60) %>% ## THE REAL EURASIAN POLYGON
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
if(!opt$clean & file.exists(out("DATA/ASIA_rast_etopo5.tif"))){
  ASIA_rast_etopo5 <- raster(out("DATA/ASIA_rast_etopo5.tif"))
}else{
  
  # Get the Natural Earth country lakes data
  dir.create(out("DATA/NaturalEarth/"), showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",destdir=out("DATA/NaturalEarth/"))
  unzip(out("DATA/NaturalEarth/ne_10m_lakes.zip"), exdir=out("DATA/NaturalEarth/ne_10m_lakes/"))
  ne_10m_lakes <- out("DATA/NaturalEarth/ne_10m_lakes/ne_10m_lakes.shp") %>%
    sf::st_read() %>%
    crop_custom(ASIA_poly)
  
  # Get the Natural Earth country boundaries data
  dir.create(out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes/"), showWarnings = FALSE, recursive = TRUE)
  FedData::download_data(url="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries_lakes.zip",destdir=out("DATA/NaturalEarth/"))
  unzip(out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes.zip"), exdir=out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes/"))
  ne_10m_admin_0_countries_lakes <- out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp") %>%
    sf::st_read() %>%
    crop_custom(ASIA_poly) %>%
    dplyr::filter(!(NAME %in% c("Indonesia",
                                "Scarborough Reef",
                                "Malaysia",
                                "Philippines",
                                "Maldives",
                                "Spratly Is.",
                                "Oman",
                                "United Arab Emirates",
                                "Saudi Arabia")))
  
  data("ETOPO5")
  ASIA_rast_etopo5 <- ETOPO5 %>%
    t() %>%
    raster(xmn = 0,
           xmx = 360,
           ymn = -90,
           ymx = 90,
           crs = CRS("+proj=longlat +ellps=clrk66 +no_defs")) %>%
    raster::crop(y = sp::spTransform(ASIA_poly, CRSobj = raster::projection(.))) %>%
    raster::mask(ne_10m_admin_0_countries_lakes %>%
                   sf::st_transform("+proj=longlat +ellps=clrk66 +no_defs") %>%
                   as("Spatial")) %>%
    raster::mask(ne_10m_lakes %>%
                   sf::st_transform("+proj=longlat +ellps=clrk66 +no_defs") %>%
                   as("Spatial"), 
                 inverse = TRUE) %T>%
    writeRaster(filename = out("DATA/ASIA_rast_etopo5.tif"),
                datatype="INT2S",
                options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND", "PHOTOMETRIC=MINISWHITE"),
                overwrite=T,
                setStatistics=FALSE)
  rm(ETOPO5)
  # unlink(out("DATA/NaturalEarth/ne_10m_lakes/"), recursive = T, force = T)
  # unlink(out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes/"), recursive = T, force = T)
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
# Here, we sample from -20 to 20 SD, at 1 SD interval
sample.points <- -20:20

# Read in data on different crop GDD needs
crop_GDD <- readr::read_csv("./DATA/blah") %>%
  dplyr::filter(crop %in% c("foxtail_millet",
                            "broomcorn_millet",
                            "wheat",
                            "barley",
                            "buckwheat"))

# create the cluster for parallel computation
cl <- makeCluster(opt$cores, type = "PSOCK")
registerDoParallel(cl)

# Transform GHCN data to GDDs of each base, and modulate to Marcott
GDDs <- sort(unique(crop_GDD$t_base))
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
gdd_model_files <- out("MODELS/",crop_GDD$cultivar,"_models.rds")

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
                                   GHCN.GDD.incremented.sd[[as.character(crop_GDD_run[crop,"t_base"])]] %>%
                                     dplyr::mutate(GDD_thresh = {GDD >= as.numeric(crop_GDD_run[crop,"min_gdd"])}) %>%
                                     dplyr::group_by(SD_change) %>%
                                     dplyr::do(out_preds = krige_and_predict(.)) %$%
                                     out_preds %>%
                                     sapply("[[","prediction") %>%
                                     apply(1,smooth.preds) %>%
                                     tibble::tibble(model = .) %>%
                                     maptools::spCbind(ASIA_rast_etopo5.sp,.) %>%
                                     readr::write_rds(out("MODELS/",crop_GDD_run[crop,"cultivar"],"_models.rds"), compress = "xz")
                                   
                                   return(out("MODELS/",crop_GDD_run[crop,"cultivar"],"_models.rds"))
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

gdd.recons <- foreach::foreach(crop = crop_GDD$cultivar,
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
                                                                                       format = "CDF",
                                                                                       datatype = "INT1S",
                                                                                       varname = "niche_probability",
                                                                                       varunit = "unitless", 
                                                                                       longname = "Probability of being in the crop niche x 100",
                                                                                       xname = "Longitude",
                                                                                       yname = "Latitude",
                                                                                       zname = "Years BP",
                                                                                       zunit = "Years BP",
                                                                                       compression = 9,
                                                                                       overwrite = TRUE)
                                                                 
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



##### BEGIN COMBINE LIKE CROP NICHES #####
## Combining crop niches from similar crops by taking an arithmatic mean
message("Combining like crop niches")
time_check <-  Sys.time()

combine_varieties <- function(x, file_tail){
  if(file.exists(out("RECONS/All_",x$crop[[1]],"_",file_tail,".nc"))) return(NULL)
  n_crops <- length(x$crop)
  foreach::foreach(cultivar = x$cultivar) %do% {
    out("RECONS/",cultivar,"_",file_tail,".nc") %>%
      raster::brick() %>%
      raster::getValues()
  } %>%
    Reduce(f = "+", x = .) %>%
    magrittr::divide_by(n_crops) %>%
    round() %>%
    raster::setValues(raster::brick(out("RECONS/",x$cultivar[[1]],"_",file_tail,".nc")), .) %>%
    raster::writeRaster(out("RECONS/All_",x$crop[[1]],"_",file_tail,".nc"),
                        format = "CDF",
                        datatype = "INT1S",
                        varname = "niche_probability",
                        varunit = "unitless", 
                        longname = "Probability of being in the crop niche x 100",
                        xname = "Longitude",
                        yname = "Latitude",
                        zname = "Years BP",
                        zunit = "Years BP",
                        compression = 9,
                        overwrite = TRUE)
  return(NULL)
}

# create the cluster for parallel computation
cl <- makeCluster(min(opt$cores,
                      crop_GDD %$%
                        crop %>%
                        unique() %>%
                        length()),
                  type = "PSOCK")
registerDoParallel(cl)

# Get mean niche for Marcott predictions
foreach::foreach(crop = crop_GDD %>%
                   split(as.factor(crop_GDD$crop)),
                 .packages = c("magrittr",
                               "foreach"),
                 .combine = c) %dopar% {
                   combine_varieties(crop, file_tail = "Z") # Get mean niche for Marcott
                   combine_varieties(crop, file_tail = "Z_Upper") # Get mean niche for Marcott upper CI
                   combine_varieties(crop, file_tail = "Z_Lower") # Get mean niche for Marcott lower CI
                 }

# stop the cluster (will free memory)
stopCluster(cl)

message("Combining like crop niches complete: ", capture.output(Sys.time() - time_check))

##### END COMBINE LIKE CROP NICHES #####



##### BEGIN PLOT CULTIVAR NICHE THROUGH TIME #####

## Plotting cultivar niche
# create the cluster for parallel computation
message("Plotting cultivar niche reconstructions")
time_check <-  Sys.time()

gdd.recons <- foreach::foreach(n = 1:nrow(crop_GDD),
                               .combine = c) %do% {
                                 
                                 cultivar <- crop_GDD[n,]$cultivar
                                 
                                 title <- stringr::str_c(crop_GDD[n,]$cultivar_long,
                                                         " — T_base: ",
                                                         crop_GDD[n,]$t_base,
                                                         "°C, Required GDD: ",
                                                         crop_GDD[n,]$min_gdd)
                                 
                                 if(file.exists(out("FIGURES/",cultivar,".pdf")) & file.exists(out("FIGURES/",cultivar,".mov")))
                                   return(out("FIGURES/",cultivar,".pdf"))
                                 
                                 rast <- raster::brick(out("RECONS/",cultivar,"_Z.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 rast.lower <- raster::brick(out("RECONS/",cultivar,"_Z_Lower.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 rast.upper <- raster::brick(out("RECONS/",cultivar,"_Z_Upper.nc")) %>%
                                   magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
                                   raster:::readAll() %>%
                                   magrittr::divide_by(100) %>%
                                   magrittr::extract2(nlayers(.):1)
                                 
                                 years <- rast %>%
                                   names() %>%
                                   gsub(pattern = "X",
                                        replacement = "",
                                        x = .) %>%
                                   as.numeric()
                                 
                                 pal <- c(rev(colorRampPalette(brewer.pal(9, "Blues")[2:9],
                                                               bias = 2,
                                                               space = "Lab")(76)),
                                          colorRampPalette(brewer.pal(9, "Reds")[2:9],
                                                           bias = 1.5,
                                                           space = "Lab")(26))
                                 
                                 if(!file.exists(out("FIGURES/",cultivar,".pdf")))
                                   space_time_plot(the_brick = rast, 
                                                   the_brick_lower = rast.lower, 
                                                   the_brick_upper = rast.upper, 
                                                   out_file = out("FIGURES/",cultivar,".pdf"),
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
                                 
                                 if(!file.exists(out("FIGURES/",cultivar,".mov")))
                                   space_time_video(the_brick = rast, 
                                                    the_brick_lower = rast.lower, 
                                                    the_brick_upper = rast.upper, 
                                                    out_file = out("FIGURES/",cultivar,".mov"),
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
                                 
                                 return(out("FIGURES/",cultivar,".pdf"))
                               }

message("Plotting of cultivar niche reconstructions complete: ", capture.output(Sys.time() - time_check))

##### END PREDICT CULTIVAR NICHE THROUGH TIME #####



##### BEGIN CHRONOMETRIC ANALYSIS #####
message("Beginning chronometric co-analysis")

# Read in the site/chronometric data
chronometric_data <- readr::read_csv("DATA/chronometric_data.csv",
                                     col_types = cols(
                                       .default = col_logical(),
                                       Site = col_character(),
                                       Period = col_integer(),
                                       Longitude = col_double(),
                                       Latitude = col_double(),
                                       `Exclude?` = col_logical(),
                                       `Notes` = col_character(),
                                       `Lab sample identifier` = col_character(),
                                       Material = col_character(),
                                       `14C age BP` = col_integer(),
                                       `1-sigma uncertainty` = col_integer(),
                                       `Age range (BC/AD)` = col_character(),
                                       `Age range lower (BP)` = col_integer(),
                                       `Age range upper (BP)` = col_integer(),
                                       Reference = col_character()
                                     )) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(`Exclude?` = ifelse(is.na(`Exclude?`), FALSE, `Exclude?`)) %>%
  dplyr::filter(!is.na(Longitude),
                !is.na(Latitude),
                !(is.na(`14C age BP`) & is.na(`Age range lower (BP)`)),
                !is.na(`14C date on cereal?`),
                !`Exclude?`) %>%
  dplyr::select(-`Exclude?`, -Notes) %>%
  group_by(Site,Period)


# Filter out sites not in study area
chronometric_names <- names(chronometric_data)
chronometric_data %<>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE) %>%
  sf::st_set_crs("+proj=longlat") %>%
  sf::st_intersection(ASIA_poly %>%
                        sf::st_as_sf() %>%
                        sf::st_transform("+proj=longlat") %>%
                        sf::st_geometry()) %>%
  sf::st_intersection(out("DATA/NaturalEarth/ne_10m_admin_0_countries_lakes/ne_10m_admin_0_countries_lakes.shp") %>%
                        sf::st_read()  %>%
                        crop_custom(ASIA_poly) %>%
                        dplyr::filter(!(NAME %in% c("Indonesia",
                                                    "Scarborough Reef",
                                                    "Malaysia",
                                                    "Philippines",
                                                    "Maldives",
                                                    "Spratly Is.",
                                                    "Oman",
                                                    "United Arab Emirates",
                                                    "Saudi Arabia"))) %>%
                        sf::st_union() %>%
                        sf::st_transform("+proj=longlat")) %>%
  dplyr::as_data_frame() %>%
  dplyr::select(-geoms) %>%
  magrittr::set_names(chronometric_names)

# Write table of dates
chronometric_data %>%
  dplyr::mutate(`Estimated age range (BP)` = stringr::str_c(`Age range lower (BP)`,"–",`Age range upper (BP)`)) %>%
  dplyr::select(Site,
                Period,
                `Lab sample identifier`,
                Material,
                `14C age BP`,
                `1-sigma uncertainty`,
                `Estimated age range (BP)`,
                Reference) %>%
  # dplyr::filter(!is.na(`14C age BP`)) %>%
  dplyr::arrange(Site, Period) %>%
  readr::write_csv(out("TABLES/sites_dates_raw.csv"))

# A function to calibrate either 14C dates (using BchronCalibrate), or 
# site age estimates (using a flat density estimation)
calibrate_density <- function(x){
  if(!is.na(x$`14C age BP`)){
    out_calib <- Bchron::BchronCalibrate(ages = x$`14C age BP`,
                                         ageSds = x$`1-sigma uncertainty`,
                                         calCurves = 'intcal13') %>%
      magrittr::extract2(1)
    out_calib <- out_calib[c("ageGrid","densities")]
  } else {
    out_calib <- list() 
    out_calib$ageGrid <- x$`Age range lower (BP)` : x$`Age range upper (BP)`
    out_calib$densities <- rep(1 / (x$`Age range lower (BP)` - x$`Age range upper (BP)`), length(out_calib$ageGrid))
  }
  return(out_calib)
}

# A function to extract density estimates over a vector of ages (years)
extract_density <- function(dens, vect){
  out_extract <- rep(0,length(vect))
  out_extract[which(vect %in% dens$ageGrid)] <- dens$densities[which(dens$ageGrid %in% vect)]
  return(out_extract)
}

densities <- chronometric_data %>%
  dplyr::select(Site,
                Period,
                `14C age BP`,
                `1-sigma uncertainty`,
                `Age range lower (BP)`,
                `Age range upper (BP)`) %>%
  purrr::by_row(calibrate_density, # Generate probability densities for 14C dates and age ranges
                .to = "Density") %>%
  dplyr::mutate(Prediction = purrr::map(Density, # Extract probabilities for Marcott years
                                        extract_density,
                                        vect = marcott2013$YearBP)) %>%
  dplyr::select(Site, Period, Prediction) %>%
  dplyr::group_by(Site, Period) %>%
  purrr::by_slice(map,~ Reduce(x = .x, f = "+"), .to = "Density") %>% # Sum probabilities over sites and periods (as in Oxcal SUM command)
  dplyr::mutate(Density = map(Density, "Prediction"),
                Density = map(Density, ~ .x / sum(.x, na.rm = T))) # re-normalize to 1


# Plot each site's probablility distribution
junk <- foreach(site = unique(densities$Site)) %do% {
  if(!file.exists(out("SITE_DENSITIES/",site,".png"))){
    png(out("SITE_DENSITIES/",site,".png"))
    g <- densities %>%
      dplyr::filter(`Site` == site) %$%
      Density %>%
      magrittr::set_names(1:length(.)) %>%
      as_tibble() %>%
      mutate(`Years BP` = marcott2013$YearBP) %>%
      tidyr::gather(Period, Density, num_range("",1:(ncol(.)-1))) %>%
      ggplot2::ggplot(aes(x = `Years BP`,
                          y = Density,
                          colour = Period)) + 
      ggplot2::geom_line() +
      ggplot2::xlim(6000,0) +
      ggplot2::ggtitle(site)
    print(g)
    dev.off()
  }
}

# # or, plot them all together with
# plot(1,type = "n",ylim = c(0,0.05), xlim = c(6000,1))
# for(site in unique(densities$`Site`)){
#   densities %>%
#     dplyr::filter(`Site` == site) %$%
#     Density %>%
#     magrittr::extract2(1) %>%
#     lines(x = marcott2013$YearBP,
#           y = .,
#           xlab = "Year BP",
#           ylab = "Density")
# }

# # Summing across all distributions yields the net distribution
# densities %$%
#   Density %>%
#   do.call(cbind,.) %>%
#   {rowSums(.)/ncol(.)} %>%
#   magrittr::multiply_by(20) %>%
#   plot(x = marcott2013$YearBP,
#        y = .,
#        xlim = c(6000,1),
#        type = "l",
#        xlab = "Year BP",
#        ylab = "Density")


# Create a spatial object of the sites
sites <- chronometric_data %>%
  mutate(foxtail_millet = ifelse(is.na(`Foxtail millet`), FALSE, TRUE),
         broomcorn_millet = ifelse(is.na(`Broomcorn millet`), FALSE, TRUE),
         wheat = ifelse(is.na(Wheat), FALSE, TRUE),
         barley = ifelse(is.na(Barley), FALSE, TRUE),
         buckwheat = ifelse(is.na(Buckwheat), FALSE, TRUE)) %>%
  dplyr::select(Site,
                Period,
                Longitude,
                Latitude,
                foxtail_millet,
                broomcorn_millet,
                wheat,
                barley,
                buckwheat) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  tidyr::gather(Crop, Present, -Site, -Period, -Longitude, -Latitude) %>%
  dplyr::filter(Present) %>%
  dplyr::select(-Present) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Site, Period) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude")) %>%
  sf::st_set_crs("+proj=longlat")

# A function to extract niches for each crop
extract_niches <- function(x){
  out_niche <- raster::brick(out("RECONS/All_",x$Crop[[1]],"_Z.nc")) %>%
    raster:::readAll() %>%
    raster::extract(x %>%
                      as("Spatial")) %>%
    split(row(.))
  x %<>% 
    set_class(c("tbl_df", "tbl", "data.frame")) %>%
    dplyr::mutate(Niche = out_niche) %>%
    sf::st_as_sf()
  return(x)
}

# Extract niches for each crop
niches <- sites %>%
  split(as.factor(sites$Crop)) %>%
  purrr::map(extract_niches) %>%
  purrr::map(~ set_class(., c("tbl_df", "tbl", "data.frame"))) %>%
  do.call(what = rbind, args = .) %>%
  sf::st_as_sf()

# A function to extract the 95% confidence interval of a distribution
get_CI <- function(dens){
  dens[is.na(dens)] <- 0
  cum_dens <- dens %>%
    cumsum() %>%
    magrittr::divide_by(sum(dens, na.rm = T))
  lower <- which(cum_dens >= 0.025)[1]
  mid <- which(cum_dens >= 0.5)[1]
  upper <- which(cum_dens >= 0.975)[1]
  return(list(Density = dens, Median = mid, CI = c(lower = lower, upper = upper)))
}

# A function to calculate local niches
local_niche <- function(niche, dens){
  if(is.na(dens$CI[["lower"]]) | is.na(dens$CI[["upper"]])) return(NULL)
  out_niche <- list()
  out_niche$Niche <- rep(NA, length(niche))
  out_niche$Niche[dens$CI[["lower"]]:dens$CI[["upper"]]] <- niche[dens$CI[["lower"]]:dens$CI[["upper"]]]
  out_niche$Niche <- out_niche$Niche / 100 # onvert to probability
  out_niche$Median <- quantile(out_niche$Niche, probs = 0.5, na.rm = TRUE)
  out_niche$CI <- c(quantile(out_niche$Niche, probs = 0.025, na.rm = TRUE),
                    quantile(out_niche$Niche, probs = 0.975, na.rm = TRUE))
  names(out_niche$CI) <- c("lower","upper")
  return(out_niche)
}

# Get the "Local" niche densities, by multiplying the site occupation probability densities
# by the crop niche probabilities (summing will get the average)
# Join the niche and density tables
niche_densities <- niches %>%
  set_class(c("tbl_df", "tbl", "data.frame")) %>%
  dplyr::left_join(densities, by = c("Site", "Period")) %>%
  dplyr::select(Site:Niche, Density) %>%
  # Get local densities
  dplyr::mutate(Density = purrr::map(Density, get_CI),
                Niche = purrr::map2(Niche, Density, local_niche)
  )

# Create biplots of each crop/site
junk <- foreach::foreach(crop = unique(niche_densities$Crop)) %do% {
  pdf(out("FIGURES/",crop,"_crossplot.pdf"))
  p <- niche_densities %>%
    dplyr::filter(Crop == crop) %>%
    dplyr::filter(!purrr::map_lgl(Niche, is.null)) %>%
    dplyr::mutate(Density_Median = marcott2013$YearBP[purrr::map_int(Density, "Median")],
                  Density_Lower = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                       purrr::map_dbl("lower")],
                  Density_Upper = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                       purrr::map_dbl("upper")],
                  Crop_Median = purrr::map_dbl(Niche, "Median"),
                  Crop_Lower = purrr::map(Niche, "CI") %>% 
                    purrr::map_dbl("lower"),
                  Crop_Upper = purrr::map(Niche, "CI") %>% 
                    purrr::map_dbl("upper")) %>%
    ggplot2::ggplot(aes(x = Density_Median, y = Crop_Median, label = Site)) + 
    geom_point(na.rm = TRUE) + 
    geom_errorbarh(aes(xmin = Density_Lower,
                       xmax = Density_Upper),
                   na.rm = TRUE) +
    geom_errorbar(aes(ymin = Crop_Lower,
                      ymax = Crop_Upper),
                  na.rm = TRUE) + 
    xlim(6000,0) +
    ylim(0,1) +
    xlab("Years BP") +
    ylab(stringr::str_c("Probability of Being in the ",crop," Niche"))
  print(p)
  dev.off()
  
  # pp <- plotly::ggplotly(tooltip = c("Site"))
  # htmlwidgets::saveWidget(widget = as_widget(pp),
  #                         file = paste0(crop,"_crossplot.html"))
}

p <- niche_densities %>%
  dplyr::mutate(`Site, Period` = stringr::str_c(Site,ifelse(is.na(Period),"",stringr::str_c(", ",Period)))) %>%
  dplyr::filter(!purrr::map_lgl(Niche, is.null)) %>%
  dplyr::mutate(Density_Median = marcott2013$YearBP[purrr::map_int(Density, "Median")],
                Density_Lower = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                     purrr::map_dbl("lower")],
                Density_Upper = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                     purrr::map_dbl("upper")],
                Crop_Median = purrr::map_dbl(Niche, "Median"),
                Crop_Lower = purrr::map(Niche, "CI") %>% 
                  purrr::map_dbl("lower"),
                Crop_Upper = purrr::map(Niche, "CI") %>% 
                  purrr::map_dbl("upper")) %>%
  ggplot2::ggplot(aes(x = Density_Median,
                      y = Crop_Median,
                      colour = Crop,
                      label = `Site, Period`)) + 
  geom_point(na.rm = TRUE) + 
  geom_errorbarh(aes(xmin = Density_Lower,
                     xmax = Density_Upper),
                 na.rm = TRUE) +
  geom_errorbar(aes(ymin = Crop_Lower,
                    ymax = Crop_Upper),
                na.rm = TRUE) + 
  xlim(6000,0) +
  ylim(0,1) +
  xlab("Years BP") +
  ylab(stringr::str_c("Probability of Being in the Niche"))
print(p)
dev.off()

pp <- plotly::ggplotly(tooltip = c("label"))
htmlwidgets::saveWidget(widget = as_widget(pp),
                        file = "All_crossplot.html")
file.copy("All_crossplot.html",
          to = out("FIGURES/All_crossplot.html"),
          overwrite = TRUE)
unlink("All_crossplot.html")

# Write out a table
niche_densities %>%
  dplyr::filter(!purrr::map_lgl(Niche, is.null)) %>%
  dplyr::mutate(`Age median (BP)` = marcott2013$YearBP[purrr::map_int(Density, "Median")],
                `Age lower CI (BP)` = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                           purrr::map_dbl("lower")],
                `Age upper CI (BP)` = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                           purrr::map_dbl("upper")],
                `Niche probability median` = purrr::map_dbl(Niche, "Median"),
                `Niche probability lower CI` = purrr::map(Niche, "CI") %>% 
                  purrr::map_dbl("lower"),
                `Niche probability upper CI` = purrr::map(Niche, "CI") %>% 
                  purrr::map_dbl("upper")) %>%
  dplyr::select(-Niche:-Density) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Crop = dplyr::recode_factor(Crop,
                                            foxtail_millet = "Foxtail millet",
                                            broomcorn_millet = "Broomcorn millet",
                                            wheat = "Wheat",
                                            barley = "Barley",
                                            buckwheat = "Buckwheat")) %>%
  dplyr::arrange(Site, Period, Crop) %>%
  readr::write_csv(out("TABLES/age_niche_estimates.csv"))

##### END CHRONOMETRIC ANALYSIS #####



##### BEGIN PLOT CROP NICHE THROUGH TIME #####

## Plotting crop niche with associated sites
# create the cluster for parallel computation
message("Plotting crop niche reconstructions")
time_check <-  Sys.time()

# Combine the sites with the density data
site_densities <- niche_densities %>%
  dplyr::filter(!purrr::map_lgl(Niche, is.null)) %>%
  dplyr::mutate(Density_Lower = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                     purrr::map_dbl("lower")],
                Density_Upper = marcott2013$YearBP[purrr::map(Density, "CI") %>% 
                                                     purrr::map_dbl("upper")]) %>%
  dplyr::select(-Density) %>%
  dplyr::filter(!is.na(Density_Lower)) %>%
  dplyr::left_join(sites, by = c("Site", "Period", "Crop")) %>%
  dplyr::mutate(Niche = purrr::map(Niche, function(x){
    x$Niche <-
      tibble::tibble(`Year BP` = marcott2013$YearBP, Niche = x$Niche)
    return(x)
  }))

crops <- crop_GDD %>%
  dplyr::filter(crop %in% site_densities$Crop) %>%
  dplyr::select(crop_long, crop) %>%
  dplyr::distinct()

for(n in 1:nrow(crops)){
  
  crop <- crops[n,]$crop
  
  title <- stringr::str_c(crops[n,]$crop_long)
  
  if(file.exists(out("FIGURES/All_",crop,".pdf")) & file.exists(out("FIGURES/All_",crop,".mov")))
    next
  
  rast <- raster::brick(out("RECONS/All_",crop,"_Z.nc")) %>%
    magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
    raster:::readAll() %>%
    magrittr::divide_by(100) %>%
    magrittr::extract2(nlayers(.):1)
  
  rast.lower <- raster::brick(out("RECONS/All_",crop,"_Z_Lower.nc")) %>%
    magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
    raster:::readAll() %>%
    magrittr::divide_by(100) %>%
    magrittr::extract2(nlayers(.):1)
  
  rast.upper <- raster::brick(out("RECONS/All_",crop,"_Z_Upper.nc")) %>%
    magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
    raster:::readAll() %>%
    magrittr::divide_by(100) %>%
    magrittr::extract2(nlayers(.):1)
  
  years <- rast %>%
    names() %>%
    gsub(pattern = "X",
         replacement = "",
         x = .) %>%
    as.numeric()
  
  pal <- c(rev(colorRampPalette(brewer.pal(9, "Blues")[2:9],
                                bias = 2,
                                space = "Lab")(76)),
           colorRampPalette(brewer.pal(9, "Reds")[2:9],
                            bias = 1.5,
                            space = "Lab")(26))
  
  if(!file.exists(out("FIGURES/All_",crop,".pdf")))
    space_time_plot(the_brick = rast, 
                    the_brick_lower = rast.lower, 
                    the_brick_upper = rast.upper, 
                    out_file = out("FIGURES/All_",crop,".pdf"),
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
  
  
  if(!file.exists(out("FIGURES/All_",crop,".mov"))){
    # A function to extract site locations for a given year and crop, and plot them
    sites_plot <- function(years = NULL, crops = NULL, scale = scales::area_pal(c(0,1))){
      x_plot <- site_densities
      year_idx <- which(marcott2013$YearBP == years)
      
      # if(!is.null(years)){
      #   x_plot %<>%
      #     dplyr::filter(any(Density_Lower < years),
      #                   any(Density_Upper > years))
      # }
      
      if(!is.null(crops)){
        x_plot %<>%
          dplyr::filter(Crop %in% crops)
      }
      
      cexs <- purrr::map(x_plot$Niche,c("Niche","Niche")) %>%
        purrr::map(year_idx) %>%
        unlist() %>%
        scale()
      
      x_plot %<>%
        dplyr::filter(!is.na(cexs))
      
      cexs <- cexs[!is.na(cexs)]
      
      x_plot %$%
        plot(geometry,
             cex = cexs,
             pch = 1,
             lwd = 3,
             col = "white",
             add = T)
      x_plot %$%
        plot(geometry,
             cex = cexs,
             pch = 1,
             lwd = 1.5,
             col = "black",
             add = T)
    }
    
    sites_legend <- function(){
      legend("bottomright",
             title = "Site probability \nof being in niche",
             legend = seq(0,1,0.2),
             pch = 1,
             pt.lwd = 1.5,
             col = "black",
             pt.cex = scales::area_pal(c(0.5,3))(seq(0,1,0.2)),
             bty = "n",
             y.intersp = 1.5,
             cex = 0.8,
             text.font = 2
      )
    }
    
    space_time_video(the_brick = rast, 
                     the_brick_lower = rast.lower, 
                     the_brick_upper = rast.upper, 
                     out_file = out("FIGURES/All_",crop,".mov"),
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
                     zcolors = pal,
                     extra_plot_fun = purrr::partial(sites_plot,
                                                     crops = crop,
                                                     scale = scales::area_pal(c(0.5,3))),
                     extra_legend_fun = sites_legend
    )
    
  }
}

message("Plotting of crop niche reconstructions complete: ", capture.output(Sys.time() - time_check))

##### END PREDICT CROP NICHE THROUGH TIME #####

# Output sessionInfo()
unlink(out("session_info.txt"))
devtools::session_info() %>%
  capture.output() %>%
  readr::write_lines(out("session_info.txt"))

message("asian_niche.R complete! Total run time: ", capture.output(Sys.time() - start_time))

### END SCRIPT ###