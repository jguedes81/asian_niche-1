# Get Mann et al. 2008 infilled instrumental temperature data, 
# and extract the 1961--1990 period.
message("Downloading Mann et al. 2008 infilled instrumental temperature data.")
dir.create("../DATA/mann2008/", showWarnings = FALSE, recursive = TRUE)
curl_download(url="ftp://ftp.ncdc.noaa.gov/pub/data/paleo/contributions_by_author/mann2008/instrument.zip",destdir="../DATA/mann2008/")
unzip("../DATA/mann2008/instrument.zip", exdir="../DATA/mann2008")
# Get just the Northern Hemisphere HAD CRU V3 data.
iHAD_NH_reform <- read.table("../DATA/mann2008/iHAD_NH_reform")
names(iHAD_NH_reform) <- c("Year","Temperature")
iHAD_NH_reform <- iHAD_NH_reform[iHAD_NH_reform$Year %in% 1961:1990,]
message("Calculating standard deviation for 1961--1990 calibration period.")
calib.sd <- sd(iHAD_NH_reform$Temperature)

# Get the Marcott et al. 2013 Northern Hemisphere temperature reconstruction, 
# which is referenced to the Mann et al. 2008 infilled instrumental average from 1961--1990
message("Downloading Marcott et al. 2013 Northern Hemisphere temperature reconstruction.")
dir.create("../DATA/marcott2013/", showWarnings = FALSE, recursive = TRUE)
curl_download(url="http://www.sciencemag.org/content/suppl/2013/03/07/339.6124.1198.DC1/Marcott.SM.database.S1.xlsx",destdir="../DATA/marcott2013/")
marcott2013 <- read.xls("../DATA/marcott2013/Marcott.SM.database.S1.xlsx", sheet="TEMPERATURE STACKS", skip=2)[,c("Age..yrs.BP..1","X90.30N...C.","X1σ.uncertainty...C..8")]
names(marcott2013) <- c("YearBP","Temperature","Uncertainty")
message("Transforming temperature deviations to standard scores.")
marcott2013 <- marcott2013[(marcott2013$YearBP <= 5510)&(!is.na(marcott2013$Temperature)),]
marcott2013$Z_Lower <- (marcott2013$Temperature-marcott2013$Uncertainty)/calib.sd
marcott2013$Z <- marcott2013$Temperature/calib.sd
marcott2013$Z_Upper <- (marcott2013$Temperature+marcott2013$Uncertainty)/calib.sd

# Write the final Marcott dataset for the reconstruction
message("Exporting the standard score data for later use.")
write.csv(marcott2013,"../OUTPUT/MARCOTT2013_Z.csv", row.names=F)