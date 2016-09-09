# This function extracts elevation data from the SRTM
# for a set of locations specified in a SpatialPointsDataFrame,
# and appends the elevation data to the SPDF.
srtmExtractor <- function(locations, SRTM.dir){
  ## We need elevation data for the stations.
  ## We extract elevations from the SRTM dataset.
  # First, get a list of SRTM tiles in the SRTM directory
  files <- list.files(SRTM.dir,pattern="\\.tif$",full.names=T,recursive=T,include.dirs=F)
  # Create a list of rasters
  tiles <- lapply(files,raster)
  # Now, extract station elevation data
  extractions <- lapply(tiles,function(tile){
    cat("Extracting elevation data from",tile@file@name,'\n')
    raster::extract(tile,locations, method='simple')
  })
  # Bind the resulting vectors to get a single vector of elevations
  elevations <- rowSums(do.call(cbind,extractions),na.rm=T)
  # Add the elevations to the GHCN.stations data
  locations$elevation <- elevations
  return(locations)
}