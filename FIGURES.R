setwd("~/Dropbox/WeatherData/NETP/R/")

# Download the FedData package, and load it.
# install.packages("devtools")
devtools::install_github("bocinsky/FedData")
library(FedData)
pkg_test("raster")
pkg_test("png")
pkg_test("RColorBrewer")
pkg_test("gdata")
pkg_test("Bchron")
pkg_test("data.table")

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+08,maxmemory=2e+09)

# Read in data on different crop GDD needs
crop_GDD <- read.csv("../DATA/crop_GDD_needs.csv")

# Get Mann et al. 2008 infilled instrumental temperature data, 
# and extract the 1961--1990 period.
dir.create("../DATA/mann2008/", showWarnings = FALSE, recursive = TRUE)
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

# Define the study region
TIBET.extent <- extent(75,105,25,41)
TIBET.poly <- polygon_from_extent(TIBET.extent,proj4string="+proj=longlat +datum=WGS84")

# ETOPO 1 for background
# Read in the ETOPO1 raster
ETOPO1.rast <- raster("../DATA/ETOPO/ETOPO1_Bed_g_geotiff.tif")
ETOPO1.rast <- raster::crop(ETOPO1.rast,TIBET.poly, snap="out")
projection(ETOPO1.rast) <- projection(TIBET.poly)

# Only use region between 1500 and 4000 m in elevation
TIBET <- (ETOPO1.rast>1500 & ETOPO1.rast<=4000)
TIBET <- calc(TIBET,function(x){x[x==0] <- NA; return(x)})

# Calculate a hillshade of the raster, for plotting
slope <- terrain(ETOPO1.rast, opt='slope')
aspect <- terrain(ETOPO1.rast, opt='aspect')
ETOPO1.hill <- hillShade(slope, aspect, 40, 230)



#### BEGIN FIGURES ####
## Figures in Science are either 2.25, 4.75, or 7.25 in wide
## Four figures/tables in a report, 6 in an article

# TIBET background
plot.ratio <- 1.57
fig.width <- 7.25
fig.height <- fig.width/plot.ratio

quartz(file='../FIGURES/TIBET_Background.png', width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(TIBET.extent),xmax(TIBET.extent)), ylim=c(ymin(TIBET.extent),ymax(TIBET.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(ETOPO1.hill, maxpixels=ncell(ETOPO1.hill), col=grey(60:100/100), useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TIBET, maxpixels=ncell(TIBET), col="#2ca25f80", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
background <- readPNG('../FIGURES/TIBET_Background.png')


# OVERVIEW: Tibetan Plateau with 1500--3500 m elevation band, and dated sites --- two columns
# Geopolitical boundaries
# Modern cities
# Rivers
# Sites in text
# Bounding boxes for Chen et al. and Guedes and Butler
countries <- readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_admin_0_countries_lakes/","ne_10m_admin_0_countries_lakes")
countries <- as(countries,"SpatialLines")
countries <- raster::crop(countries,TIBET.poly)
provinces <- raster::crop(readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lakes/","ne_10m_admin_1_states_provinces_lakes"),TIBET.poly)
provinces <- provinces[provinces$admin=="China",]
provinces.lines <- raster::crop(readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lines/","ne_10m_admin_1_states_provinces_lines"),TIBET.poly)
provinces.lines <- provinces.lines[provinces.lines$adm0_name=="China",]
rivers <- raster::crop(readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_rivers_lake_centerlines/","ne_10m_rivers_lake_centerlines"),TIBET.poly)
lakes <- raster::crop(readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_lakes/","ne_10m_lakes"),TIBET.poly)
cities <- raster::crop(readOGR("/Volumes/DATA/NATURAL_EARTH/ne_10m_populated_places/","ne_10m_populated_places"),TIBET.poly)
cities <- cities[cities$NAME %in% c("Delhi","Agra","Lucknow","Varanasi","Patna","Pokhara","Lhasa","Kunming","Chengdu","Xining"),]

sites <- read.xls("../DATA/C14-DATES-COOR-NEW.xlsx", stringsAsFactors=F)
sites <- data.table(sites)
sites <- sites[,list(Region,Site,Has_Dates=!is.na(Age_BP),Has_Grains=grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",Material), Long,Lat)]
sites <- sites[!is.na(Long) & Has_Dates]
sites <- SpatialPointsDataFrame(sites[,list(Long,Lat)],data=sites)

plot.ratio <- 1.57
plot.width <- 7.25
plot.height <- plot.width/plot.ratio

pdf(file='../FIGURES/OVERVIEW.pdf', width=plot.width, height=plot.height, bg="white", pointsize=8, version="1.7")
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', font=2, lend='round',ljoin='round')

par(mai=c(0,0,0,0), oma=c(0,0,0,0), lend='round', ljoin='round', xpd=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(TIBET.extent),xmax(TIBET.extent)), ylim=c(ymin(TIBET.extent),ymax(TIBET.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(background, xleft=xmin(TIBET.extent), xright=xmax(TIBET.extent), ybottom=ymin(TIBET.extent), ytop=ymax(TIBET.extent), interpolate=F)


plot(rivers, add=T, col='cornflowerblue')
plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)


plot(sites[!sites$Has_Grains,], pch=21, col='white', bg='white', cex=1, lwd=0.75, add=T)
plot(sites[!sites$Has_Grains,], pch=21, bg='white', cex=0.9, lwd=0.75, add=T)

plot(sites[sites$Has_Grains,], pch=21, col='white', bg='white', cex=1, lwd=0.75, add=T)
plot(sites[sites$Has_Grains,], pch=21, bg='black', cex=0.9, lwd=0.75, add=T)

plot(cities, pch=22, bg='white', add=T)
text(cities@coords,labels=cities$NAME, pos=c(1,2,4,4,2,3,3,2,4,2))
# maptools::pointLabel(cities@coords, lables=cities$NAME)

# NETP Boundary
plot(extent(94,104.5,34,39), lwd=2, lty=2, add=T)
text(94, 39, labels='NETP', adj=c(-0.1,1.5), cex=1.25)

# SETP Boundary
plot(extent(94,104.5,26,34), lwd=2, lty=2, add=T)
text(94, 34, labels='SETP', adj=c(-0.1,1.5), cex=1.25)

dev.off()

distill('../FIGURES/OVERVIEW.pdf')




# TEMPERATURE_RECONS + Radiocarbon dates: Marcott/Mann/Yang overlay/panel --- 1 column
# Read in the uncalibrated radiocarbon dates
dates <- read.xls("../DATA/C14-DATES-COOR-NEW.xlsx", stringsAsFactors=F)
dates <- data.table(dates)
dates <- dates[!is.na(Age_BP)]


### THE BCHRON WAY
BchronDensity.table <- function(x, dates=NULL, samp=1000){
  n = length(x$calAges)
  thetaRange = range(x$calAges[[1]]$ageGrid)
  for(i in 2:n) thetaRange = range(c(thetaRange,x$calAges[[i]]$ageGrid))
  if(is.null(dates)){
    dateGrid = seq(round(thetaRange[1]*0.9,3),round(thetaRange[2]*1.1,3),length=samp)
  }else{
    dateGrid <- dates
  }
  
  gauss <- function(x, mu, sig) {
    u <- (x - mu)/sig
    y <- exp(-u * u/2)
    y
  }
  
  gbase <- function(x, mus) {
    sig <- (mus[2] - mus[1])/2
    G <- outer(x, mus, gauss, sig)
    G
  }
  
  Gstar = gbase(dateGrid,x$mu)
  
  # Create the densities
  dens = vector(length=length(dateGrid))
  for(i in 1:nrow(x$p)) {
    dens = dens + Gstar%*%x$p[i,]
  }
  densFinal = dens/sum(dens)
  
  return(data.table(year=dateGrid, density=densFinal[,1]))
}

## COMMENTED OUT FOR SPEED (preprocessed and cached)
# BchronDensity.NETP.Millets <- BchronDensity(numMix=max(2,length(unique(dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.NETP.BarleyWheat <- BchronDensity(numMix=max(2,length(unique(dates[Region=="NETP" & grepl("Wheat|Barley",dates$Material),Site]))), ages=dates[Region=="NETP" & grepl("Wheat|Barley",dates$Material),Age_BP], ageSds=dates[Region=="NETP" & grepl("Wheat|Barley",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="NETP" & grepl("Wheat|Barley",dates$Material)])))
# BchronDensity.NETP.Other <- BchronDensity(numMix=max(2,length(unique(dates[Region=="NETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[Region=="NETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[Region=="NETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="NETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.NETP.All <- BchronDensity(numMix=max(2,length(unique(dates[Region=="NETP",Site]))), ages=dates[Region=="NETP",Age_BP], ageSds=dates[Region=="NETP",Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="NETP"])))
# 
# BchronDensity.SETP.Millets <- BchronDensity(numMix=max(2,length(unique(dates[Region=="SETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[Region=="SETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[Region=="SETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="SETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.SETP.BarleyWheat <- BchronDensity(numMix=max(2,length(unique(dates[Region=="SETP" & grepl("Wheat|Barley",dates$Material),Site]))), ages=dates[Region=="SETP" & grepl("Wheat|Barley",dates$Material),Age_BP], ageSds=dates[Region=="SETP" & grepl("Wheat|Barley",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="SETP" & grepl("Wheat|Barley",dates$Material)])))
# BchronDensity.SETP.Other <- BchronDensity(numMix=max(2,length(unique(dates[Region=="SETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[Region=="SETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[Region=="SETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="SETP" & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.SETP.All <- BchronDensity(numMix=max(2,length(unique(dates[Region=="SETP",Site]))), ages=dates[Region=="SETP",Age_BP], ageSds=dates[Region=="SETP",Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="SETP"])))
# 
# BchronDensity.ALL.Millets <- BchronDensity(numMix=max(2,length(unique(dates[grepl("Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[grepl("Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[grepl("Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[grepl("Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.ALL.BarleyWheat <- BchronDensity(numMix=max(2,length(unique(dates[grepl("Wheat|Barley",dates$Material),Site]))), ages=dates[grepl("Wheat|Barley",dates$Material),Age_BP], ageSds=dates[grepl("Wheat|Barley",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[grepl("Wheat|Barley",dates$Material)])))
# BchronDensity.ALL.Other <- BchronDensity(numMix=max(2,length(unique(dates[!grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Site]))), ages=dates[!grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[!grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[!grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])))
# BchronDensity.ALL.All <- BchronDensity(numMix=max(2,length(unique(dates[,Site]))), ages=dates[,Age_BP], ageSds=dates[,Uncertainty], calCurves=rep('intcal13',nrow(dates)))
# 
# BchronDensity.NETP.Millets.density <- BchronDensity.table(BchronDensity.NETP.Millets, dates=2000:5500)
# BchronDensity.NETP.BarleyWheat.density <- BchronDensity.table(BchronDensity.NETP.BarleyWheat, dates=2000:5500)
# BchronDensity.NETP.Other.density <- BchronDensity.table(BchronDensity.NETP.Other, dates=2000:5500)
# BchronDensity.NETP.All.density <- BchronDensity.table(BchronDensity.NETP.All, dates=2000:5500)
# BchronDensity.NETP.final <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE), list(BchronDensity.NETP.All.density, BchronDensity.NETP.Millets.density, BchronDensity.NETP.BarleyWheat.density, BchronDensity.NETP.Other.density))
# setnames(BchronDensity.NETP.final, c("years","All","Millets","BarleyWheat","Other"))
# saveRDS(BchronDensity.NETP.final,"../OUTPUT/BchronDensity.NETP.final.Rds", compress="xz")
# 
# BchronDensity.SETP.Millets.density <- BchronDensity.table(BchronDensity.SETP.Millets, dates=2000:5500)
# BchronDensity.SETP.BarleyWheat.density <- BchronDensity.table(BchronDensity.SETP.BarleyWheat, dates=2000:5500)
# BchronDensity.SETP.Other.density <- BchronDensity.table(BchronDensity.SETP.Other, dates=2000:5500)
# BchronDensity.SETP.All.density <- BchronDensity.table(BchronDensity.SETP.All, dates=2000:5500)
# BchronDensity.SETP.final <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE), list(BchronDensity.SETP.All.density, BchronDensity.SETP.Millets.density, BchronDensity.SETP.BarleyWheat.density, BchronDensity.SETP.Other.density))
# setnames(BchronDensity.SETP.final, c("years","All","Millets","BarleyWheat","Other"))
# saveRDS(BchronDensity.SETP.final,"../OUTPUT/BchronDensity.SETP.final.Rds", compress="xz")
# 
# BchronDensity.ALL.Millets.density <- BchronDensity.table(BchronDensity.ALL.Millets, dates=2000:5500)
# BchronDensity.ALL.BarleyWheat.density <- BchronDensity.table(BchronDensity.ALL.BarleyWheat, dates=2000:5500)
# BchronDensity.ALL.Other.density <- BchronDensity.table(BchronDensity.ALL.Other, dates=2000:5500)
# BchronDensity.ALL.All.density <- BchronDensity.table(BchronDensity.ALL.All, dates=2000:5500)
# BchronDensity.ALL.final <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE), list(BchronDensity.ALL.All.density, BchronDensity.ALL.Millets.density, BchronDensity.ALL.BarleyWheat.density, BchronDensity.ALL.Other.density))
# setnames(BchronDensity.ALL.final, c("years","All","Millets","BarleyWheat","Other"))
# saveRDS(BchronDensity.ALL.final,"../OUTPUT/BchronDensity.ALL.final.Rds", compress="xz")

BchronDensity.NETP.final <- readRDS("../OUTPUT/BchronDensity.NETP.final.Rds")
BchronDensity.SETP.final <- readRDS("../OUTPUT/BchronDensity.SETP.final.Rds")
BchronDensity.ALL.final <- readRDS("../OUTPUT/BchronDensity.ALL.final.Rds")

### THE OXCAL WAY
# A function to calculate OxCal-like summed probability distributions
OxCal.SUM <- function(ages){
  years <- unlist(lapply(ages,'[[','ageGrid'))
  densities <- unlist(lapply(ages,'[[','densities'))
  year.dens <- data.table(years=years,densities=densities)
  year.dens.sums <- year.dens[,sum(densities),by=years]
  setkey(year.dens.sums, years)
  return(year.dens.sums)
}

remove_nas = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)),i:=0,with=FALSE]
}

## COMMENTED OUT FOR SPEED (preprocessed and cached)
# # Generate the calibrations usng the IntCal13 Northern Hemisphere calibration data
# ages <- BchronCalibrate(ages=dates[,Age_BP], ageSds=dates[,Uncertainty], calCurves=rep('intcal13',nrow(dates)))
# # bchronDensity.Millets <- BchronDensity(ages=dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Age_BP], ageSds=dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material),Uncertainty], calCurves=rep('intcal13',nrow(dates[Region=="NETP" & grepl("Foxtail|Broomcorn|Millet",dates$Material)])))
# 
# # Generate regional/species summed probability distributions
# SUM.NETP.all <- OxCal.SUM(ages[dates$Region=='NETP'])[years %in% 5500:2000,list(years,density=V1)]
# SUM.NETP.Millets <- OxCal.SUM(ages[dates$Region=='NETP' & grepl("Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.NETP.BarleyWheat <- OxCal.SUM(ages[dates$Region=='NETP' & grepl("Wheat|Barley",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.NETP.Other <- OxCal.SUM(ages[dates$Region=='NETP' & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.NETP.final <- Reduce(function(x, y) merge(x, y, by = "years", all = TRUE), list(SUM.NETP.all, SUM.NETP.Millets, SUM.NETP.BarleyWheat, SUM.NETP.Other))
# setnames(SUM.NETP.final, c("years","All","Millets","BarleyWheat","Other"))
# remove_nas(SUM.NETP.final)
# SUM.NETP.final <- SUM.NETP.final[,list(years,All=All/sum(All),Millets=Millets/sum(All),BarleyWheat=BarleyWheat/sum(All),Other=Other/sum(All))]
# saveRDS(SUM.NETP.final,"../OUTPUT/SUM.NETP.final.Rds", compress="xz")
# 
# 
# SUM.SETP.Millets <- OxCal.SUM(ages[dates$Region=='SETP' & grepl("Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.SETP.BarleyWheat <- OxCal.SUM(ages[dates$Region=='SETP' & grepl("Wheat|Barley",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.SETP.Other <- OxCal.SUM(ages[dates$Region=='SETP' & !grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.SETP.all <- OxCal.SUM(ages[dates$Region=='SETP'])[years %in% 5500:2000,list(years,density=V1)]
# SUM.SETP.final <- Reduce(function(x, y) merge(x, y, by = "years", all = TRUE), list(SUM.SETP.all, SUM.SETP.Millets, SUM.SETP.BarleyWheat, SUM.SETP.Other))
# setnames(SUM.SETP.final, c("years","All","Millets","BarleyWheat","Other"))
# remove_nas(SUM.SETP.final)
# SUM.SETP.final <- SUM.SETP.final[,list(years,All=All/sum(All),Millets=Millets/sum(All),BarleyWheat=BarleyWheat/sum(All),Other=Other/sum(All))]
# saveRDS(SUM.SETP.final,"../OUTPUT/SUM.SETP.final.Rds", compress="xz")
# 
# 
# SUM.ALL.Millets <- OxCal.SUM(ages[grepl("Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.ALL.BarleyWheat <- OxCal.SUM(ages[grepl("Wheat|Barley",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.ALL.Other <- OxCal.SUM(ages[!grepl("Wheat|Barley|Foxtail|Broomcorn|Millet",dates$Material)])[years %in% 5500:2000,list(years,density=V1)]
# SUM.ALL.all <- OxCal.SUM(ages)[years %in% 5500:2000,list(years,density=V1)]
# SUM.ALL.final <- Reduce(function(x, y) merge(x, y, by = "years", all = TRUE), list(SUM.ALL.all, SUM.ALL.Millets, SUM.ALL.BarleyWheat, SUM.ALL.Other))
# setnames(SUM.ALL.final, c("years","All","Millets","BarleyWheat","Other"))
# remove_nas(SUM.ALL.final)
# SUM.ALL.final <- SUM.ALL.final[,list(years,All=All/sum(All),Millets=Millets/sum(All),BarleyWheat=BarleyWheat/sum(All),Other=Other/sum(All))]
# saveRDS(SUM.ALL.final,"../OUTPUT/SUM.ALL.final.Rds", compress="xz")

SUM.NETP.final <- readRDS("../OUTPUT/SUM.NETP.final.Rds")
SUM.SETP.final <- readRDS("../OUTPUT/SUM.SETP.final.Rds")
SUM.ALL.final <- readRDS("../OUTPUT/SUM.ALL.final.Rds")


colors <- c("#00339999","#CC330099")
solid.colors <- c("#003399","#CC3300")

phi <- (1+sqrt(5))/2
mai <- c(0.25,0.25,0,0)
fig.width <- 4.75
fig.height <- 4.75
margins <- 0.5
between <- 0.25
numplots <- 4
plot.height <- (fig.height-(margins*1.25)-(between*3))/numplots
legend.cex <- 0.75

# A final graph of the mean of the series through time, with a smoothing spline
quartz(file="../FIGURES/TEMPS_C14.pdf", width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='pdf', family="Gulim", pointsize=8, dpi=600)
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold', lend="round", ljoin='round')

par(mai=c(margins,margins*1.5,margins*0.25,margins/2))
plot(1, type='n', xaxs="i",yaxs="i", xlab="", ylab="", ylim=c(0,1),xlim=c(5500,2000), axes=FALSE, main='')

i <- 1
par(mai=c(margins+(plot.height*(4-i))+(between*(4-i)),margins*1.5,(plot.height*(i-1))+(between*(i-1))+(margins*0.25),margins/2), new=T, xpd=F)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(5500,2000), ylim=c(-0.2,1), axes=FALSE, main='')
polygon(col="gray75",border=NA,x=c(rev(marcott2013$YearBP),marcott2013$YearBP), y=c(rev(marcott2013$Temperature+marcott2013$Uncertainty),marcott2013$Temperature-marcott2013$Uncertainty))
lines(Temperature~YearBP,data=marcott2013, lwd=2)
mtext("Anomaly", side=2, line=4.25, cex=1.2)
mtext("(°C)", side=2, line=3, cex=1.2)
text(x=2000,y=1,label="A", cex=2, adj=c(0,0.5), xpd=T)
axis(2,las=1)
axis(1, labels=F)

i <- 2
par(mai=c(margins+(plot.height*(4-i))+(between*(4-i)),margins*1.5,(plot.height*(i-1))+(between*(i-1))+(margins*0.25),margins/2), new=T, xpd=T)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(5500,2000), ylim=c(0,0.0008), axes=FALSE, main='')

polygon(col="gray80",border=NA,x=c(SUM.NETP.final[,years],rev(SUM.NETP.final[,years])), y=c(SUM.NETP.final[,All],rep(0,length(SUM.NETP.final[,All]))))
polygon(col=colors[[1]],border=NA,x=c(SUM.NETP.final[,years],rev(SUM.NETP.final[,years])), y=c(SUM.NETP.final[,Millets],rep(0,length(SUM.NETP.final[,Millets]))))
polygon(col=colors[[2]],border=NA,x=c(SUM.NETP.final[,years],rev(SUM.NETP.final[,years])), y=c(SUM.NETP.final[,BarleyWheat],rep(0,length(SUM.NETP.final[,BarleyWheat]))))

lines(y=BchronDensity.NETP.final[,All*sum(SUM.NETP.final[,All])], x=BchronDensity.NETP.final[,years], col='black', lwd=2)
lines(y=BchronDensity.NETP.final[,Millets*sum(SUM.NETP.final[,Millets])], x=BchronDensity.NETP.final[,years], col=solid.colors[[1]], lwd=2)
lines(y=BchronDensity.NETP.final[,BarleyWheat*sum(SUM.NETP.final[,BarleyWheat])], x=BchronDensity.NETP.final[,years], col=solid.colors[[2]], lwd=2)
mtext("NETP", side=2, line=4.25, cex=1.2)
mtext("(density x 1000)", side=2, line=3, cex=1.2)
text(x=2000,y=0.0008,label="B", cex=2, adj=c(0,0.5), xpd=T)
axis(2,at=seq(0,0.0008,0.0002), labels=seq(0,0.0008,0.0002)*1000, las=1)
axis(1, labels=F)

i <- 3
par(mai=c(margins+(plot.height*(4-i))+(between*(4-i)),margins*1.5,(plot.height*(i-1))+(between*(i-1))+(margins*0.25),margins/2), new=T, xpd=T)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(5500,2000), ylim=c(0,0.0008), axes=FALSE, main='')
polygon(col="gray80",border=NA,x=c(SUM.SETP.final[,years],rev(SUM.SETP.final[,years])), y=c(SUM.SETP.final[,All],rep(0,length(SUM.SETP.final[,All]))))
polygon(col=colors[[1]],border=NA,x=c(SUM.SETP.final[,years],rev(SUM.SETP.final[,years])), y=c(SUM.SETP.final[,Millets],rep(0,length(SUM.SETP.final[,Millets]))))
polygon(col=colors[[2]],border=NA,x=c(SUM.SETP.final[,years],rev(SUM.SETP.final[,years])), y=c(SUM.SETP.final[,BarleyWheat],rep(0,length(SUM.SETP.final[,BarleyWheat]))))

lines(y=BchronDensity.SETP.final[,All*sum(SUM.SETP.final[,All])], x=BchronDensity.SETP.final[,years], col='black', lwd=2)
lines(y=BchronDensity.SETP.final[,Millets*sum(SUM.SETP.final[,Millets])], x=BchronDensity.SETP.final[,years], col=solid.colors[[1]], lwd=2)
lines(y=BchronDensity.SETP.final[,BarleyWheat*sum(SUM.SETP.final[,BarleyWheat])], x=BchronDensity.SETP.final[,years], col=solid.colors[[2]], lwd=2)

mtext("SETP", side=2, line=4.25, cex=1.2)
mtext("(density x 1000)", side=2, line=3, cex=1.2)
text(x=2000,y=0.0008,label="C", cex=2, adj=c(0,0.5), xpd=T)
axis(2,at=seq(0,0.0008,0.0002), labels=seq(0,0.0008,0.0002)*1000, las=1)
axis(1, labels=F)

i <- 4
par(mai=c(margins+(plot.height*(4-i))+(between*(4-i)),margins*1.5,(plot.height*(i-1))+(between*(i-1))+(margins*0.25),margins/2), new=T, xpd=T)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(5500,2000), ylim=c(0,0.0008), axes=FALSE, main='')
polygon(col="gray80",border=NA,x=c(SUM.ALL.final[,years],rev(SUM.ALL.final[,years])), y=c(SUM.ALL.final[,All],rep(0,length(SUM.ALL.final[,All]))))
polygon(col=colors[[1]],border=NA,x=c(SUM.ALL.final[,years],rev(SUM.ALL.final[,years])), y=c(SUM.ALL.final[,Millets],rep(0,length(SUM.ALL.final[,Millets]))))
polygon(col=colors[[2]],border=NA,x=c(SUM.ALL.final[,years],rev(SUM.ALL.final[,years])), y=c(SUM.ALL.final[,BarleyWheat],rep(0,length(SUM.ALL.final[,BarleyWheat]))))

lines(y=BchronDensity.ALL.final[,All*sum(SUM.ALL.final[,All])], x=BchronDensity.ALL.final[,years], col='black', lwd=2)
lines(y=BchronDensity.ALL.final[,Millets*sum(SUM.ALL.final[,Millets])], x=BchronDensity.ALL.final[,years], col=solid.colors[[1]], lwd=2)
lines(y=BchronDensity.ALL.final[,BarleyWheat*sum(SUM.ALL.final[,BarleyWheat])], x=BchronDensity.ALL.final[,years], col=solid.colors[[2]], lwd=2)

mtext("All samples", side=2, line=4.25, cex=1.2)
mtext("(density x 1000)", side=2, line=3, cex=1.2)
text(x=2000,y=0.0008,label="D", cex=2, adj=c(0,0.5), xpd=T)
axis(2,at=seq(0,0.0008,0.0002), labels=seq(0,0.0008,0.0002)*1000, las=1)
axis(1)

mtext("Years BP", side=1, line=2.5, cex=1.4)

par(mai=c(margins,margins*1.5,margins*0.25,margins/2), new=T, xpd=T)
plot(1, type='n', xaxs="i",yaxs="i", xlab="", ylab="", ylim=c(0,1),xlim=c(5500,2000), axes=FALSE, main='')

# plot a vertical area defining the period of overlap between Millet dates and Wheat/Barley dates
overlap <- c(min(SUM.ALL.final[Millets>0]$years), max(SUM.ALL.final[BarleyWheat>0]$years))
rect(xleft=overlap[[2]],xright=overlap[[1]],ybottom=0, ytop=1, lty=3)

dev.off()
distill("../FIGURES/TEMPS_C14.pdf")


### CROP NICHE FIGURES ###

## COMMENTED OUT FOR SPEED (preprocessed and cached)
# crops <- list.files("../MARCOTT",pattern="Z_ETOPO1")
# crops <- gsub("_Z_ETOPO1.tif","",crops)
# final_bricks_Z <- lapply(list.files("../MARCOTT",pattern="Z_ETOPO1",full.names=T), brick)
# final_bricks_Z_Lower <- lapply(list.files("../MARCOTT",pattern="Z_Lower_ETOPO1",full.names=T), brick)
# final_bricks_Z_Upper <- lapply(list.files("../MARCOTT",pattern="Z_Upper_ETOPO1",full.names=T), brick)
# 
# final_bricks_Z_ts <- lapply(final_bricks_Z, function(x){as.numeric(cellStats(x,'mean'))})
# final_bricks_Z_ts <- t(do.call(rbind,final_bricks_Z_ts))/10000
# rownames(final_bricks_Z_ts) <- marcott2013$YearBP
# colnames(final_bricks_Z_ts) <- crops
# final_bricks_Z_ts[is.na(final_bricks_Z_ts)] <- 0
# saveRDS(final_bricks_Z_ts,"../MARCOTT/final_bricks_Z_ts.Rds")
# 
# final_bricks_Z_Lower_ts <- lapply(final_bricks_Z_Lower, function(x){as.numeric(cellStats(x,'mean'))})
# final_bricks_Z_Lower_ts <- t(do.call(rbind,final_bricks_Z_Lower_ts))/10000
# rownames(final_bricks_Z_Lower_ts) <- marcott2013$YearBP
# colnames(final_bricks_Z_Lower_ts) <- crops
# final_bricks_Z_Lower_ts[is.na(final_bricks_Z_Lower_ts)] <- 0
# saveRDS(final_bricks_Z_Lower_ts,"../MARCOTT/final_bricks_Z_Lower_ts.Rds")
# 
# final_bricks_Z_Upper_ts <- lapply(final_bricks_Z_Upper, function(x){as.numeric(cellStats(x,'mean'))})
# final_bricks_Z_Upper_ts <- t(do.call(rbind,final_bricks_Z_Upper_ts))/10000
# rownames(final_bricks_Z_Upper_ts) <- marcott2013$YearBP
# colnames(final_bricks_Z_Upper_ts) <- crops
# final_bricks_Z_Upper_ts[is.na(final_bricks_Z_Upper_ts)] <- 0
# saveRDS(final_bricks_Z_Upper_ts,"../MARCOTT/final_bricks_Z_Upper_ts.Rds")

final_bricks_Z_ts <- readRDS("../MARCOTT/final_bricks_Z_ts.Rds")
final_bricks_Z_Lower_ts <- readRDS("../MARCOTT/final_bricks_Z_Lower_ts.Rds")
final_bricks_Z_Upper_ts <- readRDS("../MARCOTT/final_bricks_Z_Upper_ts.Rds")

# matplot(final_bricks_Z_ts, x=marcott2013$YearBP, type='l')
# matplot(final_bricks_Z_Lower_ts, x=marcott2013$YearBP, type='l')
# matplot(final_bricks_Z_Upper_ts, x=marcott2013$YearBP, type='l')

Barley_hiLat_mean_Z <- rowMeans(final_bricks_Z_ts[,c("Barley_Alaska","Barley_Juskiw")])
Millet_mean_Z <- rowMeans(final_bricks_Z_ts[,c("broomcorn_millet_10","broomcorn_millet_55","foxtail_millet_100-130day","foxtail_millet_110-125day","foxtail_millet_90day")])
WheatBarley_mean_Z <- rowMeans(final_bricks_Z_ts[,c("spring_barley","spring_wheat","winter_barley","winter_wheat")])

Barley_hiLat_mean_Z_Upper <- rowMeans(final_bricks_Z_Upper_ts[,c("Barley_Alaska","Barley_Juskiw")])
Millet_mean_Z_Upper <- rowMeans(final_bricks_Z_Upper_ts[,c("broomcorn_millet_10","broomcorn_millet_55","foxtail_millet_100-130day","foxtail_millet_110-125day","foxtail_millet_90day")])
WheatBarley_mean_Z_Upper <- rowMeans(final_bricks_Z_Upper_ts[,c("spring_barley","spring_wheat","winter_barley","winter_wheat")])

Barley_hiLat_mean_Z_Lower <- rowMeans(final_bricks_Z_Lower_ts[,c("Barley_Alaska","Barley_Juskiw")])
Millet_mean_Z_Lower <- rowMeans(final_bricks_Z_Lower_ts[,c("broomcorn_millet_10","broomcorn_millet_55","foxtail_millet_100-130day","foxtail_millet_110-125day","foxtail_millet_90day")])
WheatBarley_mean_Z_Lower <- rowMeans(final_bricks_Z_Lower_ts[,c("spring_barley","spring_wheat","winter_barley","winter_wheat")])



colors <- c("#00339999","#CC330099")
solid.colors <- c("#003399","#CC3300")

phi <- (1+sqrt(5))/2
mai <- c(0.25,0.25,0,0)
fig.width <- 4.75
fig.height <- fig.width/phi
margins <- 0.5
plot.height <- (fig.height-(margins*1.25))
legend.cex <- 0.75

# A final graph of the mean of the series through time, with a smoothing spline
quartz(file="../FIGURES/NICHE_COMPARE.pdf", width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='pdf', family="Gulim", pointsize=8, dpi=600)
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold', lend="round", ljoin='round')

par(mai=c(margins,margins*1.25,margins*0.25,margins*0.5), xpd=F)

plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(5500,0), ylim=c(0,1), axes=FALSE, main='')

polygon(x=c(names(Millet_mean_Z_Upper),rev(names(Millet_mean_Z_Lower))),y=c(Millet_mean_Z_Upper,rev(Millet_mean_Z_Lower)), border=NA, col=colors[1])
polygon(x=c(names(WheatBarley_mean_Z_Upper),rev(names(WheatBarley_mean_Z_Lower))),y=c(WheatBarley_mean_Z_Upper,rev(WheatBarley_mean_Z_Lower)), border=NA, col=colors[2])

# polygon(x=c(names(Barley_hiLat_mean_Z_Upper),rev(names(Barley_hiLat_mean_Z_Lower))),y=c(Barley_hiLat_mean_Z_Upper,rev(Barley_hiLat_mean_Z_Lower)), border=NA, col="yellow")

lines(Millet_mean_Z, x=names(Millet_mean_Z), col=solid.colors[1], lwd=2)
lines(WheatBarley_mean_Z, x=names(WheatBarley_mean_Z), col=solid.colors[2], lwd=2)

# lines(Barley_hiLat_mean_Z, x=names(WheatBarley_mean_Z), lwd=2, lty=2)


# plot a vertical area defining the period of overlap between Millet dates and Wheat/Barley dates
# overlap <- c(min(SUM.all.Millets[density>0.001]$years), max(SUM.all.BarleyWheat[density>0.001]$years))
# rect(xleft=overlap[[2]],xright=overlap[[1]],ybottom=0, ytop=1, lty=3)

par(lend=1)
legend("bottomleft", lty=1, lwd=12, col=rev(colors), legend=c('',''), bty='n', cex=1.2)
legend("bottomleft", col=rev(solid.colors), lwd=2, legend=rev(c('Millets ± 1σ','Wheat/Barley ± 1σ')), bty='n', border=NA, cex=1.2)

mtext("Mean Niche Probability", side=2, line=3, cex=1.4)
axis(2,las=1)
axis(1, at=seq(0,5500,500), labels=c(0,'',1000,'',2000,'',3000,'',4000,'',5000,''))

mtext("Years BP", side=1, line=2.5, cex=1.4)

dev.off()

distill("../FIGURES/NICHE_COMPARE.pdf")



# NICHE_COMPARE_MAPS: Panel of maps comparing Wheat/Barley versus Millet niches, for zoomed in area east of 90 degrees, 3000 BP, 3750 BP, 4500 BP --- 2 column
# We are going to do 6 maps, comparing the niche extent for millets versus wheat/barley at 3010, 3750, and 4510 BP

crops <- list.files("../MARCOTT",pattern="Z_ETOPO1")
crops <- gsub("_Z_ETOPO1.tif","",crops)
final_bricks_Z <- lapply(list.files("../MARCOTT",pattern="Z_ETOPO1",full.names=T), brick)
final_bricks_Z <- lapply(final_bricks_Z,function(x){subset(x,subset=which(marcott2013$YearBP %in% c(3010,3750,4510)))})

Millet_Mean_Z_rasts <- ((final_bricks_Z[[3]]+final_bricks_Z[[4]]+final_bricks_Z[[6]]+final_bricks_Z[[7]]+final_bricks_Z[[8]])/5)/10000
WheatBarley_Mean_Z_rasts <- ((final_bricks_Z[[9]]+final_bricks_Z[[10]]+final_bricks_Z[[11]]+final_bricks_Z[[12]])/4)/10000

ETP.extent <- extent(94,104.5,26,39)
ETP.poly <- polygon_from_extent(ETP.extent,proj4string="+proj=longlat +datum=WGS84")
Millet_Mean_Z_rasts <- crop(Millet_Mean_Z_rasts, ETP.poly, snap='out')
WheatBarley_Mean_Z_rasts <- crop(WheatBarley_Mean_Z_rasts, ETP.poly, snap='out')
ETOPO1.hill.ETP <- crop(ETOPO1.hill, ETP.poly, snap='out')

# TIBET background
plot.ratio <- 0.685
fig.width <- 7.25
between <- 0.1
nrow <- 2
ncol <- 3
legend.height <- 0.65
year.height <- 0.3
plot.width <- (fig.width - (ncol-1)*between - year.height)/ncol
plot.height <- plot.width/plot.ratio
fig.height <- (plot.height * nrow) + legend.height + year.height
alphas <- "80"
colors <- rev(brewer.pal(10,"RdYlBu"))
colors <- paste0(colors,alphas)
color.breaks <- seq(from=0.5,to=1,by=0.5/length(colors))



quartz(file='../FIGURES/ETP_Background.png', width=plot.width, height=plot.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(ETOPO1.hill.ETP, maxpixels=ncell(ETOPO1.hill.ETP), col=grey(60:100/100), useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
ETP.background <- readPNG('../FIGURES/ETP_Background.png')



pdf(file='../FIGURES/NICHE_COMPARE_MAPS.pdf', width=fig.width, height=fig.height, bg="white", pointsize=8, version="1.7")
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', font=2, lend='round',ljoin='round')

par(mai=c(0,0,0,0), lend='round', ljoin='round', xpd=T)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(0,1), ylim=c(0,1), axes=FALSE, main='')

loc <- c(1,1)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(Millet_Mean_Z_rasts[[3]], zlim=c(0.5,1), maxpixels=ncell(Millet_Mean_Z_rasts[[3]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="A", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  


loc <- c(1,2)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)


quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(Millet_Mean_Z_rasts[[2]], zlim=c(0.5,1), maxpixels=ncell(Millet_Mean_Z_rasts[[2]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)


# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="B", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  

loc <- c(1,3)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(Millet_Mean_Z_rasts[[1]], zlim=c(0.5,1), maxpixels=ncell(Millet_Mean_Z_rasts[[1]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="C", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  

loc <- c(2,1)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height+between),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(WheatBarley_Mean_Z_rasts[[3]], zlim=c(0.5,1), maxpixels=ncell(WheatBarley_Mean_Z_rasts[[3]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="D", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  

loc <- c(2,2)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height+between),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(WheatBarley_Mean_Z_rasts[[2]], zlim=c(0.5,1), maxpixels=ncell(WheatBarley_Mean_Z_rasts[[2]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="E", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  

loc <- c(2,3)
par(mai=c(legend.height + (nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-(nrow-1))*(plot.height+between),(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(ETP.background, xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

quartz(file='../FIGURES/temp.png', width=plot.width, height=plot.height, antialias=FALSE, bg="transparent", type='png', family="Gulim", pointsize=8, dpi=300)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ETP.extent),xmax(ETP.extent)), ylim=c(ymin(ETP.extent),ymax(ETP.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(WheatBarley_Mean_Z_rasts[[1]], zlim=c(0.5,1), maxpixels=ncell(WheatBarley_Mean_Z_rasts[[1]]), breaks=color.breaks, col=colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
rasterImage(readPNG('../FIGURES/temp.png'), xleft=xmin(ETP.extent), xright=xmax(ETP.extent), ybottom=ymin(ETP.extent), ytop=ymax(ETP.extent), interpolate=F)

# plot(rivers, add=T, col='cornflowerblue')
# plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

inch <- (ETP.extent@xmax-ETP.extent@xmin)/(fig.width-par('mai')[2]-par('mai')[4])
# scalebar.new(d=100, cex=2, font=2, side='left',lab.side='left', height=0.075*inch, label="", line.offset=c(-0.05*inch,0.05*inch), xy=c(xmax(ETP.extent),ymin(ETP.extent)), lwd=4, lend=1)
text(x=xmin(ETP.extent)+(0.075*inch), y=ymax(ETP.extent)-(0.075*inch), labels="F", col="black", font=2, cex=3, adj=c(0,1), xpd=T)  


## LEGENDS

loc <- 1
par(mai=c(0,year.height+((ncol-1)-(ncol-loc))*(plot.width+between),(nrow*plot.height) + year.height,(ncol-loc)*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=range(color.breaks), ylim=c(0,0.65), xaxs="i", yaxs="i", axes=FALSE, main='')
rect(xleft=color.breaks[1], ybottom=0.45, xright=color.breaks[length(color.breaks)], ytop=0.55, col="gray90", border=NA)
rect(xleft=color.breaks[1:(length(color.breaks)-1)], ybottom=0.45, xright=color.breaks[2:length(color.breaks)], ytop=0.55, col=colors, border=NA)
text(x=seq(0.5,1,0.1),y=0.4,labels=c("0.5","0.6","0.7","0.8","0.9","1.0"),adj=c(0.5,1),xpd=T)
text(x=0.75, y=0.25, labels="Probability of being in the niche", adj=c(0.5,1),xpd=T, cex=1.5)

## YEARS
loc <- 1
par(mai=c(legend.height+(plot.height*2),year.height+((ncol-1)-(ncol-loc))*(plot.width+between),0,(ncol-loc)*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(0,1), ylim=c(0,0.3), xaxs="i", yaxs="i", axes=FALSE, main='')
text(x=0.5,y=0.1,labels="4510 BP",cex=2, adj=c(0.5,0), xpd=T)

loc <- 2
par(mai=c(legend.height+(plot.height*2),year.height+((ncol-1)-(ncol-loc))*(plot.width+between),0,(ncol-loc)*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(0,1), ylim=c(0,0.3), xaxs="i", yaxs="i", axes=FALSE, main='')
text(x=0.5,y=0.1,labels="3750 BP",cex=2, adj=c(0.5,0), xpd=T)

loc <- 3
par(mai=c(legend.height+(plot.height*2),year.height+((ncol-1)-(ncol-loc))*(plot.width+between),0,(ncol-loc)*(plot.width+between)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(0,1), ylim=c(0,0.3), xaxs="i", yaxs="i", axes=FALSE, main='')
text(x=0.5,y=0.1,labels="3010 BP",cex=2, adj=c(0.5,0), xpd=T)

## CROPS
par(mai=c(legend.height+plot.height+between,0,year.height,(plot.width*3)+(between*2)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(0,0.3), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, main='')
text(x=0.2,y=0.5,labels="Millets",cex=2, adj=c(0.5,0), xpd=T, srt=90)

par(mai=c(legend.height,0,year.height+plot.height+between,(plot.width*3)+(between*2)), xpd=F, new=T)
plot(1, type='n', xlab="", ylab="",xlim=c(0,0.3), ylim=c(0,1), xaxs="i", yaxs="i", axes=FALSE, main='')
text(x=0.2,y=0.5,labels="Wheat & Barley",cex=2, adj=c(0.5,0), xpd=T, srt=90)

dev.off()

distill('../FIGURES/NICHE_COMPARE_MAPS.pdf')

#### SI Figures/Tables ####

# MODERN_COMPARE: Comparison between modern cropping locations and modern wheat/barley niche

# ALASKAN_BARLEY: Maps/graphs of Alaskan barley niche extent

# MODULATION: Details on variance modulation and climatology determination

# CROP_GDD_REQs: Table of crop GDD requirements

# RADIOCARBON: Table of radiocarbon dates

# 

