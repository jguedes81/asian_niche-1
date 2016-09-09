## SET THE WORKING DIRECTORY TO THE LOCATION WHERE YOU UNZIPPED THIS SOURCE CODE
# setwd("~/directory/of/source/R/")
setwd("~/Dropbox/WeatherData/NETP/R/")

# Download the FedData package, and load it.
# THIS CODE WILL ONLY WORK WITH VERSION 1.1.0 OF FedData
install.packages("FedData")
library(FedData)
pkgTest("gdata")
pkgTest("png")
pkgTest("RColorBrewer")

# Load all functions
all.functions <- lapply(list.files("./src",full.names=T),source)

## DEFINE THE TIBETAN PLATEAU ##
# Define the study region
# Define the study region
TIBET.extent <- extent(75,105,25,41)
TIBET.poly <- polygonFromExtent(TIBET.extent,proj4string="+proj=longlat +datum=WGS84")

# Run the script that downloads and crops the ETOPO1 dataset
source("./PREPARE_ETOPO.R")

# Read in the ETOPO1 raster
ETOPO1.rast <- raster("../DATA/ETOPO1/ETOPO1_Bed_g_geotiff.tif")
ETOPO1.rast <- raster::crop(ETOPO1.rast,TIBET.poly, snap="out")
projection(ETOPO1.rast) <- projection(TIBET.poly)

# Calculate a hillshade of the raster, for plotting
slope <- terrain(ETOPO1.rast, opt='slope')
aspect <- terrain(ETOPO1.rast, opt='aspect')
ETOPO1.hill <- hillShade(slope, aspect, 40, 230)

# Create a high-resolution version of the DEM, just using nearest-neighbor (i.e., don't interpolate).
# Fortunately, the GLC-SHARE dataset is exactly 1/2 arc-minute (30 arc-second).
ETOPO1.rast.highres <- disaggregate(ETOPO1.rast,2,method='bilinear')
ETOPO1.rast.highres <- raster::crop(ETOPO1.rast.highres,TIBET.poly)

# Only select elevations greater than 1500 masl
TIBET <- (ETOPO1.rast.highres>1500)
TIBET <- calc(TIBET,function(x){x[x==0] <- NA; return(x)})

ETOPO1.rast.highres <- raster::mask(ETOPO1.rast.highres,TIBET)

# Read in the GLC-SHARE Cropland database.
# GLC-SHARE is 1/2 arc-minute (30 arc-second), or about 1 km resolution.
# This gives the approximate percent of each cell that is cropland
# Get Mann et al. 2008 infilled instrumental temperature data, 
# and extract the 1961--1990 period.
message("Downloading the ETOPO1 grid-aligned dataset.")
dir.create("../DATA/GLC-SHARE/", showWarnings = FALSE, recursive = TRUE)
curlDownload(url="http://www.fao.org/geonetwork/srv/en/http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_02.zip&access=private",destdir="../DATA/GLC-SHARE/")
GLC_SHARE.cropland <- raster('../DATA/GLC-SHARE/GlcShare_v10_02/glc_shv10_02.Tif')
GLC_SHARE.cropland <- raster::crop(GLC_SHARE.cropland, TIBET.poly)
GLC_SHARE.cropland <- raster::mask(GLC_SHARE.cropland,TIBET)
# And this one is just the categorical dominant landcover
GLC_SHARE.dominant <- raster('../DATA/GLC-SHARE/GlcShare_v10_Dominant/glc_shv10_DOM.Tif')
GLC_SHARE.dominant <- raster::crop(GLC_SHARE.dominant, TIBET.poly)
GLC_SHARE.dominant.legend <- read.csv('../DATA/GLC-SHARE/GlcShare_v10_Dominant/glc_shv10_DOM_ReadMe_Legend.txt', header=F, stringsAsFactors=F)
GLC_SHARE.dominant.cropland <- GLC_SHARE.dominant == as.numeric(GLC_SHARE.dominant.legend$V1[grepl('Cropland',GLC_SHARE.dominant.legend$V2)])
GLC_SHARE.dominant.cropland <- raster::mask(GLC_SHARE.dominant.cropland,TIBET)

# Mask the high-res DEM with the cropland.
# This gives you only DEM values that have cropland under them.
ETOPO1.dominant.cropland <- raster::mask(ETOPO1.rast.highres,GLC_SHARE.dominant.cropland, maskvalue=0)
ETOPO1.share.cropland <- raster::mask(ETOPO1.rast.highres,GLC_SHARE.cropland, maskvalue=0)

# Plot the relationship between % Cropland and elevation.
# Notice the downward slope, and that very few cells with 
# mean elevations above 4000 m have any proportion of cropland.
plot(ETOPO1.rast.highres,GLC_SHARE.cropland)

# Let's look at a histogram of the elevations of cropland-dominant cells.
# hist(ETOPO1.dominant.cropland, xlim=c(1500,6000), breaks=72, xlab='Mean m.a.s.l.', ylab="Cell count", main="Cropland Elevation")
plot(ecdf(ETOPO1.dominant.cropland[]),xlim=c(1500,max(ETOPO1.rast.highres[], na.rm=T)), xlab='Mean m.a.s.l.', ylab="Cumulative probability", main="Cropland Elevation")

phi <- (1+sqrt(5))/2
mai <- c(0.25,0.25,0,0)
fig.width <- 4.75
fig.height <- fig.width/phi
margins <- 0.5
plot.height <- (fig.height-(margins*1.25))
legend.cex <- 0.75

# A final graph of the mean of the series through time, with a smoothing spline
quartz(file="../FIGURES/MODERN_CROPLAND_ECDF_INTERP.pdf", width=fig.width, height=fig.height, antialias=FALSE, bg="white", type='pdf', family="Gulim", pointsize=8, dpi=600)
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', family='Helvetica Bold', lend="round", ljoin='round')

par(mai=c(margins,margins*1.25,margins*0.25,margins*0.5), xpd=F)

plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(1500,5000), ylim=c(0,1.05), axes=FALSE, main='')

f <- ecdf(ETOPO1.dominant.cropland[])
x <- seq(1500,5000,length.out=100000)
x95 <- mean(x[which(abs(f(x)-0.95)==min(abs(f(x)-0.95)))])

# plot(f, do.points=F, verticals=T, add=T, lwd=1.5, col='black', col.01line=adjustcolor('white',alpha.f=0))
lines(y=f(x), x=x,lwd=1.5, xpd=T)
segments(0,0.95,x95,0.95, lty=2)
segments(x95,0.95,x95,0, lty=2)
text(x=x95, y=0.1, paste0("95% of cells at < ",round(x95)," masl"),pos=4)


mtext("Cumulative probability", side=2, line=3, cex=1.4)
axis(2,at=c(0,0.2,0.4,0.6,0.8,1.0,0.95),las=1)
axis(1)
# box()
mtext("Elevation (m)", side=1, line=2.5, cex=1.4)

dev.off()

distill("../FIGURES/MODERN_CROPLAND_ECDF_INTERP.pdf")


# How many cropland-dominant cells are above 4000 m?
1-sum(ETOPO1.dominant.cropland[]>=4000, na.rm=T)/sum(ETOPO1.rast.highres[]>1500, na.rm=T) # 823 cells (i.e., ~823 sq. km). Out of ~6.9 million. ~1/100 of a percent.
1-sum(ETOPO1.dominant.cropland[]>=4000, na.rm=T)/sum(ETOPO1.rast.highres[]>=4000, na.rm=T) # Percent of cells above 4000 masl that are cropland-dominant: ~3/100 of a percent

sum(ETOPO1.share.cropland[]>=4000, na.rm=T) # 6361 cells, or ~9/100 of a percent
sum(ETOPO1.share.cropland[]>=4000, na.rm=T)/sum(ETOPO1.rast.highres[]>=4000, na.rm=T) # ~2/10 of a percent


# OVERVIEW: Tibetan Plateau with 1500--3500 m elevation band, and dated sites --- two columns
# Geopolitical boundaries
# Modern cities
# Rivers
# Sites in text
# Bounding boxes for Chen et al. and Guedes and Butler
countries <- readOGR("../DATA/NATURAL_EARTH/ne_10m_admin_0_countries_lakes/","ne_10m_admin_0_countries_lakes")
countries <- as(countries,"SpatialLines")
countries <- raster::crop(countries,TIBET.poly)
provinces <- raster::crop(readOGR("../DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lakes/","ne_10m_admin_1_states_provinces_lakes"),TIBET.poly)
provinces <- provinces[provinces$admin=="China",]
provinces.lines <- raster::crop(readOGR("../DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lines/","ne_10m_admin_1_states_provinces_lines"),TIBET.poly)
provinces.lines <- provinces.lines[provinces.lines$adm0_name=="China",]
rivers <- raster::crop(readOGR("../DATA/NATURAL_EARTH/ne_10m_rivers_lake_centerlines/","ne_10m_rivers_lake_centerlines"),TIBET.poly)
lakes <- raster::crop(readOGR("../DATA/NATURAL_EARTH/ne_10m_lakes/","ne_10m_lakes"),TIBET.poly)
cities <- raster::crop(readOGR("../DATA/NATURAL_EARTH/ne_10m_populated_places/","ne_10m_populated_places"),TIBET.poly)
cities <- cities[cities$NAME %in% c("Delhi","Agra","Lucknow","Varanasi","Patna","Pokhara","Lhasa","Kunming","Chengdu","Xining"),]


plot.ratio <- 1.57
plot.width <- 7.25
plot.height <- plot.width/plot.ratio

GLC_SHARE.dominant.cropland.notZero <- calc(GLC_SHARE.dominant.cropland,function(x){x[x==0] <- NA; return(x)})

cropland.colors <- colorRampPalette(brewer.pal(9,"YlGn")[1:9])(100)
quartz(file='../FIGURES/MODERN_CROPLAND_Background.png', width=plot.width, height=plot.height, antialias=FALSE, bg="white", type='png', family="Gulim", pointsize=8, dpi=600)
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(TIBET.extent),xmax(TIBET.extent)), ylim=c(ymin(TIBET.extent),ymax(TIBET.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(ETOPO1.hill, maxpixels=ncell(ETOPO1.hill), col=grey(60:100/100), useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(GLC_SHARE.dominant.cropland.notZero, maxpixels=ncell(GLC_SHARE.dominant.cropland.notZero), col=cropland.colors, useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
dev.off()
background <- readPNG('../FIGURES/MODERN_CROPLAND_Background.png')

pdf(file='../FIGURES/MODERN_CROPLAND_MAP.pdf', width=plot.width, height=plot.height, bg="white", pointsize=8, version="1.7")
par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', font=2, lend='round',ljoin='round')

par(mai=c(0,0,0,0), oma=c(0,0,0,0), lend='round', ljoin='round', xpd=T)
plot(1, type='n', xlab="", ylab="",xlim=c(xmin(TIBET.extent),xmax(TIBET.extent)), ylim=c(ymin(TIBET.extent),ymax(TIBET.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')
rasterImage(background, xleft=xmin(TIBET.extent), xright=xmax(TIBET.extent), ybottom=ymin(TIBET.extent), ytop=ymax(TIBET.extent), interpolate=F)

plot(rivers, add=T, col='cornflowerblue')
plot(lakes, add=T, col='cornflowerblue', border=NA)
plot(countries, add=T, lwd=2)
plot(provinces.lines, add=T, lty=2)

plot(cities, pch=22, bg='white', add=T)
text(cities@coords,labels=cities$NAME, pos=c(1,2,4,4,2,3,3,2,4,2))

dev.off()

distill('../FIGURES/MODERN_CROPLAND_MAP.pdf')


