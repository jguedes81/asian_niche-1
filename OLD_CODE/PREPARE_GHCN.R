## PREPARE_GHCN.R
## A script in support of d'Alpoim Guedes, Manning, Butler, and Bocinsky 2015.
## This script downloads and cleans daily climate records from the Global Historical Climate Database.
# It should be run from ./GUEDES_ET_AL_2015_FINAL.R

# Set the calibration period
calibration.years <- 1961:1990

# Create a polygon for GHCN extraction
GHCN.poly <- polygon_from_extent(extent(65,150,5,55), proj4string="+proj=longlat")

## GHCN DATA ##
# These are the GHCN stations that will be used to calibrate the interpolation model.
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# GHCN.data <- get_ghcn(template=GHCN.poly, label="Asia_GHCN", elements=c("tmin","tmax"), raw.dir="/Volumes/DATA/GHCN/", extraction.dir = "../EXTRACTIONS/GHCN/", standardize=T)
# saveRDS(GHCN.data,file='../OUTPUT/ghcn_data_all.Rds')
GHCN.data <- readRDS('../OUTPUT/ghcn_data_all.Rds')

GHCN.stations <- GHCN.data[[1]] # The spatial station data
GHCN.data <- GHCN.data[[2]] # The actual temperature data
GHCN.stations <- GHCN.stations[!duplicated(GHCN.stations$ID),] # Remove duplicated station location data
GHCN.data <- GHCN.data[as.character(GHCN.stations$ID)] # Ensure that the order of the two datasets are the same

# Some stations have the same "location", but not the same data. Remove these points.
nonduplicates <- !duplicated(coordinates(GHCN.stations)) * !duplicated(coordinates(GHCN.stations),fromLast=T)
GHCN.stations <- GHCN.stations[nonduplicates,]
GHCN.data <- GHCN.data[nonduplicates]

# Get run length encoding of missing data
all.rles <- do.call(c,lapply(GHCN.data,function(test){
  tryCatch(getMissingRLE(test),error=function(e) NULL)
}))

# Calculate the cutoff length of data gaps.
# Years with gaps longer than "cutoff" will be dropped
cutoff <- calcGapCutoff(rleVector=all.rles, pLevel=0.95)

## Clean the GHCN data
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# GHCN.data.clean <- lapply(GHCN.data,function(station.data){ghcnCleaner(data.list=station.data, min.years=10, year.range=calibration.years, na.cutoff=cutoff)})
# names(GHCN.data.clean) <- names(GHCN.data)
# GHCN.data.clean <- GHCN.data.clean[!sapply(GHCN.data.clean, is.null)]
# saveRDS(GHCN.data.clean,"../OUTPUT/GHCN.data.clean.Rds")
GHCN.data.clean <- readRDS("../OUTPUT/GHCN.data.clean.Rds")

# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# Keep only the clean stations
# GHCN.stations <- GHCN.stations[GHCN.stations$ID %in% names(GHCN.data.clean),]
# 
# # Get the station elevations
# ## YOU MUST POINT THIS AT A DIRECTORY WITH SRTM TILES
# GHCN.stations <- srtmExtractor(locations=GHCN.stations, SRTM.dir="/Volumes/DATA/SRTM/")
# 
# # Get all stations averages over the calibration period
# GHCN.data.averages <- lapply(GHCN.data.clean, function(station){
#   return(lapply(station,calcDailyMeanSD))
# })
# 
# # Create a final dataset for use anywhere in Asia!
# GHCN.data.final <- list(GHCN.stations,GHCN.data.averages)
# saveRDS(GHCN.data.final,file='../OUTPUT/ghcn_data_final.Rds')
GHCN.data.final <- readRDS('../OUTPUT/ghcn_data_final.Rds')


# # ### An example of running daily mean standardization of GDD
# 
# # # Weather data from Xining, China:CHM00052866
# Xining.elev <- GHCN.stations[GHCN.stations$ID=="CHM00052866","elevation"]
# Xining.tmin.raw <- data.frame(DOY=as.numeric(strftime(as.POSIXlt(paste(rep(GHCN.data.clean[['CHM00052866']][['tmin']]$YEAR,each=31),rep(GHCN.data.clean[['CHM00052866']][['tmin']]$MONTH,each=31),rep(1:31,times=nrow(GHCN.data.clean[['CHM00052866']][['tmin']])), sep='.'), format="%Y.%m.%d"), format = "%j")), DATA=as.numeric(t(GHCN.data.clean[['CHM00052866']][['tmin']][,-1:-2])) )
# Xining.tmin.raw <- Xining.tmin.raw[!is.na(Xining.tmin.raw$DATA) & !is.na(Xining.tmin.raw$DOY),]
# Xining.tmin.smooth <- calcDailyMeanSD(GHCN.data.clean[['CHM00052866']][['tmin']])
# 
# Xining.tmax.raw <- data.frame(DOY=as.numeric(strftime(as.POSIXlt(paste(rep(GHCN.data.clean[['CHM00052866']][['tmax']]$YEAR,each=31),rep(GHCN.data.clean[['CHM00052866']][['tmax']]$MONTH,each=31),rep(1:31,times=nrow(GHCN.data.clean[['CHM00052866']][['tmax']])), sep='.'), format="%Y.%m.%d"), format = "%j")), DATA=as.numeric(t(GHCN.data.clean[['CHM00052866']][['tmax']][,-1:-2])) )
# Xining.tmax.raw <- Xining.tmax.raw[!is.na(Xining.tmax.raw$DATA) & !is.na(Xining.tmax.raw$DOY),]
# Xining.tmax.smooth <- calcDailyMeanSD(GHCN.data.clean[['CHM00052866']][['tmax']])
# 
# # # Weather data from Chengdu, China:CHM00056294
# Chengdu.elev <- GHCN.stations[GHCN.stations$ID=="CHM00056294","elevation"]
# Chengdu.tmin.raw <- data.frame(DOY=as.numeric(strftime(as.POSIXlt(paste(rep(GHCN.data.clean[['CHM00056294']][['tmin']]$YEAR,each=31),rep(GHCN.data.clean[['CHM00056294']][['tmin']]$MONTH,each=31),rep(1:31,times=nrow(GHCN.data.clean[['CHM00056294']][['tmin']])), sep='.'), format="%Y.%m.%d"), format = "%j")), DATA=as.numeric(t(GHCN.data.clean[['CHM00056294']][['tmin']][,-1:-2])) )
# Chengdu.tmin.raw <- Chengdu.tmin.raw[!is.na(Chengdu.tmin.raw$DATA) & !is.na(Chengdu.tmin.raw$DOY),]
# Chengdu.tmin.smooth <- calcDailyMeanSD(GHCN.data.clean[['CHM00056294']][['tmin']])
# 
# Chengdu.tmax.raw <- data.frame(DOY=as.numeric(strftime(as.POSIXlt(paste(rep(GHCN.data.clean[['CHM00056294']][['tmax']]$YEAR,each=31),rep(GHCN.data.clean[['CHM00056294']][['tmax']]$MONTH,each=31),rep(1:31,times=nrow(GHCN.data.clean[['CHM00056294']][['tmax']])), sep='.'), format="%Y.%m.%d"), format = "%j")), DATA=as.numeric(t(GHCN.data.clean[['CHM00056294']][['tmax']][,-1:-2])) )
# Chengdu.tmax.raw <- Chengdu.tmax.raw[!is.na(Chengdu.tmax.raw$DATA) & !is.na(Chengdu.tmax.raw$DOY),]
# Chengdu.tmax.smooth <- calcDailyMeanSD(GHCN.data.clean[['CHM00056294']][['tmax']])
# 
# Xining.tmin.raw[,2] <- Xining.tmin.raw[,2]/10
# Xining.tmin.smooth[,2:3] <- Xining.tmin.smooth[,2:3]/10
# Xining.tmax.raw[,2] <- Xining.tmax.raw[,2]/10
# Xining.tmax.smooth[,2:3] <- Xining.tmax.smooth[,2:3]/10
# 
# Chengdu.tmin.raw[,2] <- Chengdu.tmin.raw[,2]/10
# Chengdu.tmin.smooth[,2:3] <- Chengdu.tmin.smooth[,2:3]/10
# Chengdu.tmax.raw[,2] <- Chengdu.tmax.raw[,2]/10
# Chengdu.tmax.smooth[,2:3] <- Chengdu.tmax.smooth[,2:3]/10
# 
# Xining.gdd.raw <- data.frame(DOY=Xining.tmin.raw[,1], DATA=calcGDD(tmin.vector=Xining.tmin.raw[,2], tmax.vector=Xining.tmax.raw[,2], t.base=0))
# Xining.gdd.smooth <- calcDailyMeanSD(Xining.gdd.raw, unwrapped=T)
# Chengdu.gdd.raw <- data.frame(DOY=Chengdu.tmin.raw[,1], DATA=calcGDD(tmin.vector=Chengdu.tmin.raw[,2], tmax.vector=Chengdu.tmax.raw[,2], t.base=0))
# Chengdu.gdd.smooth <- calcDailyMeanSD(Chengdu.gdd.raw, unwrapped=T)
# 
# 
# # TIBET background
# fig.width <- 7.25
# between <- 0.25
# nrow <- 3
# ncol <- 2
# year.height <- 0.3
# plot.width <- (fig.width - ((ncol-1)*between) - (year.height*2))/ncol
# plot.height <- plot.width
# fig.height <- (plot.height * nrow) + year.height*2
# 
# 
# pdf(file='../FIGURES/GHCN_SMOOTH_COMPARE.pdf', width=fig.width, height=fig.height, bg="white", pointsize=8, version="1.7")
# par(bg='white',fg='black',col.lab='black', col.main='black', col.axis='black', font=2, lend='round',ljoin='round')
# 
# par(mai=c(0,0,0,0), lend='round', ljoin='round', xpd=T)
# plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(0,1), ylim=c(0,1), axes=FALSE, main='')
# 
# loc <- c(1,1)
# par(pty='s',mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Xining.tmin.raw$DATA, polar.pos=(Xining.tmin.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(-20,40), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Xining.tmin.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.tmin.smooth[,'MEAN'])+as.numeric(Xining.tmin.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.tmin.smooth[,'MEAN'])-as.numeric(Xining.tmin.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# text(x=-60, y=60, labels="A", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# loc <- c(2,1)
# par(mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Xining.tmax.raw$DATA, polar.pos=(Xining.tmax.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(-20,40), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Xining.tmax.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.tmax.smooth[,'MEAN'])+as.numeric(Xining.tmax.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.tmax.smooth[,'MEAN'])-as.numeric(Xining.tmax.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# text(x=-60, y=60, labels="B", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# loc <- c(3,1)
# par(mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Xining.gdd.raw$DATA, polar.pos=(Xining.gdd.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(0,30), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Xining.gdd.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.gdd.smooth[,'MEAN'])+as.numeric(Xining.gdd.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Xining.gdd.smooth[,'MEAN'])-as.numeric(Xining.gdd.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=2, line.col='red', add=TRUE)
# text(x=-30, y=30, labels="C", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# 
# loc <- c(1,2)
# par(mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Chengdu.tmin.raw$DATA, polar.pos=(Chengdu.tmin.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(-20,40), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Chengdu.tmin.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.tmin.smooth[,'MEAN'])+as.numeric(Chengdu.tmin.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.tmin.smooth[,'MEAN'])-as.numeric(Chengdu.tmin.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# text(x=-60, y=60, labels="D", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# 
# loc <- c(2,2)
# par(mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Chengdu.tmax.raw$DATA, polar.pos=(Chengdu.tmax.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(-20,40), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Chengdu.tmax.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.tmax.smooth[,'MEAN'])+as.numeric(Chengdu.tmax.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.tmax.smooth[,'MEAN'])-as.numeric(Chengdu.tmax.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(-20,40), lwd=2, line.col='red', add=TRUE)
# text(x=-60, y=60, labels="E", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# 
# loc <- c(3,2)
# par(mai=c(year.height+(nrow-loc[1])*(plot.height+between),year.height+((ncol-1)-(ncol-loc[2]))*(plot.width+between),year.height+(loc[1]-1)*(plot.height+between),year.height+(ncol-loc[2])*(plot.width+between)), xpd=F, new=T)
# polar.plot(lengths=Chengdu.gdd.raw$DATA, polar.pos=(Chengdu.gdd.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(0,30), cex=0.1, point.symbols=19, mar=par('mar'))
# polar.plot(lengths=as.numeric(Chengdu.gdd.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=3, line.col='dodgerblue', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.gdd.smooth[,'MEAN'])+as.numeric(Chengdu.gdd.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=2, line.col='red', add=TRUE)
# polar.plot(lengths=as.numeric(Chengdu.gdd.smooth[,'MEAN'])-as.numeric(Chengdu.gdd.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,30), lwd=2, line.col='red', add=TRUE)
# text(x=-30, y=30, labels="F", col="black", font=2, cex=3, adj=c(1,0), xpd=T)
# 
# dev.off()
# 
# distill('../FIGURES/GHCN_SMOOTH_COMPARE.pdf')
# 
