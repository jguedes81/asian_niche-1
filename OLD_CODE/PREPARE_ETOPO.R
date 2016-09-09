# Get the ETOPO1 grid-aligned dataset.
message("Downloading the ETOPO1 grid-aligned dataset.")
dir.create("../DATA/ETOPO1/", showWarnings = FALSE, recursive = TRUE)
curl_download(url="http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/georeferenced_tiff/ETOPO1_Bed_g_geotiff.zip",destdir="../DATA/ETOPO1/")
unzip("../DATA/ETOPO1/ETOPO1_Bed_g_geotiff.zip", exdir="../DATA/ETOPO1")

ETOPO1.rast <- raster("../DATA/ETOPO1/ETOPO1_Bed_g_geotiff.tif")
ETOPO1.rast <- raster::crop(ETOPO1.rast,TIBET.poly, snap='out')
projection(ETOPO1.rast) <- projection(TIBET.poly)

# Only use region between 1500 and 4000 m in elevation
TIBET <- (ETOPO1.rast>1500 & ETOPO1.rast<=4000)
TIBET <- calc(TIBET,function(x){x[x==0] <- NA; return(x)})
ETOPO1.rast <- raster::mask(ETOPO1.rast,TIBET)

writeRaster(ETOPO1.rast, "../OUTPUT/TIBETAN_PLATEAU_ETOPO1.tif", datatype="INT2U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"),overwrite=T,setStatistics=FALSE)

rm(ETOPO1.rast)
rm(TIBET)
gc();gc()