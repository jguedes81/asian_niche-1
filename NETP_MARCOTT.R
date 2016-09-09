

# Read in data on different crop GDD needs
crop_GDD <- read.csv("../DATA/crop_GDD_needs.csv")

# Read in the standard scored data from Marcott et al. 2013
marcott2013 <- read.csv("../OUTPUT/MARCOTT2013_Z.csv")

# Calculate niches for each crop using Marcott 2013.
crops <- list.files("../SMOOTH", full.names=T)
crops <- crops[grep("ETOPO1", crops)]
for(Crop in crops){
  Crop.name <- basename(Crop)
  Crop.name <- gsub("_SMOOTHS_ETOPO1.Rds","",Crop.name)
  if(all(file.exists(paste0("../MARCOTT/",Crop.name,"_",c("Z_Lower","Z","Z_Upper"),"_ETOPO1.tif")))) next
  crop.data <- readRDS(Crop)
  for(Zs in c("Z_Lower","Z","Z_Upper")){
    if(file.exists(paste0("../MARCOTT/",Crop.name,"_",Zs,"_ETOPO1.tif"))) next
    out <- lapply(1:length(crop.data),function(x){as.integer(predict(crop.data[[x]],newdata=marcott2013[,Zs])*10000)})
    out <- do.call(rbind, out)
    out.rast <- brick(TIBET, nl=ncol(out), values=F)
    out.rast[!is.na(TIBET[])] <- out
    writeRaster(out.rast,paste0("../MARCOTT/",Crop.name,"_",Zs,"_ETOPO1.tif"), datatype="INT2U", options=c("COMPRESS=DEFLATE", "ZLEVEL=9", "INTERLEAVE=BAND"),overwrite=T,setStatistics=FALSE)
    print(out.rast)
  }
}
