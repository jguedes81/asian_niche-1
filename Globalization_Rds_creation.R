
# Download the FedData package, and load it.
# install.packages("devtools")
# devtools::install_github("bocinsky/FedData")
 .libPaths(c("/data/kamiak/nick.maggio/local/R_Libs","/opt/apps/r/3.2.2/lib64/R/library"))
library(FedData)
pkg_test("raster")
pkg_test("png")
pkg_test("RColorBrewer")
pkg_test("gdata")
pkg_test("Bchron")
pkg_test("data.table")

# Load all functions
all.functions <- lapply(list.files("src",full.names=T),source)

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Read in data on different crop GDD needs
crop_GDD <- read.csv("../DATA/crop_GDD_needs.csv")

# Get Mann et al. 2008 infilled instrumental temperature data, 
# and extract the 1961--1990 period.
# dir.create("../DATA/mann2008/", showWarnings = FALSE, recursive = TRUE)
# download.file("ftp://ftp.ncdc.noaa.gov/pub/data/paleo/contributions_by_author/mann2008/instrument.zip","../DATA/mann2008/instrument.zip", mode="wb")
# unzip("../DATA/mann2008/instrument.zip", exdir="../DATA/mann2008")
# Get just the Northern Hemisphere HAD CRU V3 data.
iHAD_NH_reform <- read.table("../DATA/mann2008/iHAD_NH_reform")
names(iHAD_NH_reform) <- c("Year","Temperature")
iHAD_NH_reform <- iHAD_NH_reform[iHAD_NH_reform$Year %in% 1961:1990,]
calib.sd <- sd(iHAD_NH_reform$Temperature)

# Get the Marcott et al. 2013 Northern Hemisphere temperature reconstruction, 
# which is referenced to the Mann et al. 2008 infilled instrumental average from 1961--1990
# dir.create("../DATA/marcott2013/", showWarnings = FALSE, recursive = TRUE)
# download.file("http://www.sciencemag.org/content/suppl/2013/03/07/339.6124.1198.DC1/Marcott.SM.database.S1.xlsx","../DATA/marcott2013/Marcott.SM.database.S1.xlsx", mode="wb")
marcott2013 <- read.xls("../DATA/marcott2013/Marcott.SM.database.S1.xlsx", sheet="TEMPERATURE STACKS", skip=2)[,c("Age..yrs.BP..1","X90.30N...C.","X1σ.uncertainty...C..8")]
names(marcott2013) <- c("YearBP","Temperature","Uncertainty")
marcott2013 <- marcott2013[(marcott2013$YearBP <= 5510)&(!is.na(marcott2013$Temperature)),]
marcott2013$Z_Lower <- (marcott2013$Temperature-marcott2013$Uncertainty)/calib.sd
marcott2013$Z <- marcott2013$Temperature/calib.sd
marcott2013$Z_Upper <- (marcott2013$Temperature+marcott2013$Uncertainty)/calib.sd

# Clear unused memory
gc(); gc()


# ETOPO 1 for background
# Read in the ETOPO1 raster
#ETOPO1.rast <- raster("/Volumes/DATA/ETOPO/ETOPO1_Bed_g_geotiff.tif")
#ETOPO1.rast <- raster::crop(ETOPO1.rast,SEASIA.poly, snap="out")
#projection(ETOPO1.rast) <- projection(SEASIA.poly)

crops <- list.files("../OUTPUT/MARCOTT",pattern="Z_ETOPO5")
crops <- gsub("_Z_ETOPO5.tif","",crops)
final_bricks_Z <- lapply(list.files("../OUTPUT/MARCOTT",pattern="Z_ETOPO5",full.names=T), brick)
final_bricks_Z_Lower <- lapply(list.files("../OUTPUT/MARCOTT",pattern="L_ETOPO5",full.names=T), brick)
final_bricks_Z_Upper <- lapply(list.files("../OUTPUT/MARCOTT",pattern="U_ETOPO5",full.names=T), brick)

#the line below is taking too long to calculate
final_bricks_Z_ts <- lapply(final_bricks_Z, function(x){as.numeric(cellStats(x,'mean'))})
final_bricks_Z_ts <- t(do.call(rbind,final_bricks_Z_ts))/10000
rownames(final_bricks_Z_ts) <- marcott2013$YearBP
colnames(final_bricks_Z_ts) <- crops
final_bricks_Z_ts[is.na(final_bricks_Z_ts)] <- 0
saveRDS(final_bricks_Z_ts,"../final_bricks_Z_ts.Rds")

#the line below is taking too long to calculate
final_bricks_Z_Lower_ts <- lapply(final_bricks_Z_Lower, function(x){as.numeric(cellStats(x,'mean'))})
final_bricks_Z_Lower_ts <- t(do.call(rbind,final_bricks_Z_Lower_ts))/10000
rownames(final_bricks_Z_Lower_ts) <- marcott2013$YearBP
colnames(final_bricks_Z_Lower_ts) <- crops
final_bricks_Z_Lower_ts[is.na(final_bricks_Z_Lower_ts)] <- 0
saveRDS(final_bricks_Z_Lower_ts,"../final_bricks_Z_Lower_ts.Rds")

# the line below is taking too long to calculate
final_bricks_Z_Upper_ts <- lapply(final_bricks_Z_Upper, function(x){as.numeric(cellStats(x,'mean'))})
final_bricks_Z_Upper_ts <- t(do.call(rbind,final_bricks_Z_Upper_ts))/10000
rownames(final_bricks_Z_Upper_ts) <- marcott2013$YearBP
colnames(final_bricks_Z_Upper_ts) <- crops
final_bricks_Z_Upper_ts[is.na(final_bricks_Z_Upper_ts)] <- 0
saveRDS(final_bricks_Z_Upper_ts,"../final_bricks_Z_Lower_ts.Rds")

final_bricks_Z_ts <- readRDS("../final_bricks_Z_ts.Rds")
final_bricks_Z_Lower_ts <- readRDS("../MARCOTT/final_bricks_Z_Lower_ts.Rds")
final_bricks_Z_Upper_ts <- readRDS("../MARCOTT/final_bricks_Z_Upper_ts.Rds")


