library(raster)
library(magrittr)
library(foreach)
library(RColorBrewer)

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),
                        source)

# Read in data on different crop GDD needs
crop_GDD <- readr::read_csv("./DATA/crop_GDD_needs.csv")

crop <- crop_GDD[13,]$crop

test <- raster::brick(out("RECONS/",crop,"_Z.nc")) %>%
  magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
  raster:::readAll() %>%
  magrittr::divide_by(100)

test.lower <- raster::brick(out("RECONS/",crop,"_Z_Lower.nc")) %>%
  magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
  raster:::readAll() %>%
  magrittr::divide_by(100)

test.upper <- raster::brick(out("RECONS/",crop,"_Z_Upper.nc")) %>%
  magrittr::extract2(which(.@z$`Years BP` > 1000)) %>%
  raster:::readAll() %>%
  magrittr::divide_by(100)

layer.names <- test %>%
  names() %>%
  gsub(pattern = "X", replacement = "", x = .) %>%
  as.numeric()

pal <- c(rev(colorRampPalette(brewer.pal(9, "Blues")[2:9],
                              bias = 1.5,
                              space = "Lab")(50)),
         colorRampPalette(brewer.pal(9, "Reds")[2:9],
                          bias = 1.5,
                          space = "Lab")(50))

space_time_plot(the_brick = test, 
                the_brick_lower = test.lower, 
                the_brick_upper = test.upper, 
                out_file = "./OUTPUT/test.pdf",
                title = stringr::str_c(crop_GDD[1,]$Crop_long,
                                       " — T_base: ",
                                       crop_GDD[1,]$base_t,
                                       "ºC, Required GDD: ",
                                       crop_GDD[1,]$min_gdd) ,
                time = layer.names,
                timelim = c(max(layer.names),min(layer.names)),
                timeaxis =  seq(from = max(layer.names)-500,
                                to = min(layer.names),
                                by = -500),
                timelab = "Years BP",
                zbreaks = seq(0,1,0.01),
                zlab = "Probability of being in niche",
                zaxis = seq(0,1,0.1),
                zcolors = pal
)

