distill <- function(file){
  system(paste0("gs -sDEVICE=pdfwrite -dNOPAUSE -dQUIET -dBATCH -dCompatibilityLevel=1.9 -dEmbedAllFonts=true -sOutputFile=./temp.pdf ",file))
  system(paste0("rm ",file))
  system(paste0("mv ./temp.pdf ",file))
  return()
}