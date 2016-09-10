# Create a function to split the raster using gdalUtils::gdal_translate
tile_rast <- function(infile,
                      outdir = paste0("./",basename(infile),"_tiles"),
                      n_tiles,
                      tile_width,
                      tile_height,
                      length_out_x,
                      length_out_y,
                      parallel_cl = 1) {
  
  # Create a cluster for use with doParallel
  registerDoParallel(parallel_cl)
  
  rast <- raster(infile)
  
  if(!missing(tile_width)){
    length_out_x = (xmax(rast) - xmin(rast)) / tile_width
  }
  
  rast.edges.x <- seq(xmin(rast),xmax(rast),res(rast)[1])
  rast.edges.y <- seq(ymin(rast),ymax(rast),res(rast)[2])
  
  
  
  
  rast@data
  
  
  
  x <- iris[which(iris[,5] != "setosa"), c(1,5)]
  trials <- 10000
  ptime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
      }
    })[3]
  ptime
  
  
  
  gdal_translate(infile,
                 outfile, 
                 srcwin=c(llx, lly - win_height, win_width, win_height))
}