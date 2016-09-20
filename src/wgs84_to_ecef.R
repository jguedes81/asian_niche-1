# A function to convert from WGS lon/lat/elev (geodetic) coordinates 
# to Earth-centered Earth-fixed (ECEF) position
wgs84_to_ecef <- function(lon, lat, elev){
  lon <- lon * pi/180 # lon in degrees
  lat <- lat * pi/180 # lat in degrees
  
  # WGS84 parameters
  a <- 6378137 # semi-major axis
  f <- 1/298.257223563 # flattening
  b <- a * (1-f) # semi-minor axis
  a_sq <- a*a
  b_sq <- b*b
  e <- sqrt((a_sq-b_sq)/a_sq)
  
  N <- a / sqrt(1 - ((e^2) * sin(lat) * sin(lat)))
  
  x <- (N + elev) * cos(lat) * cos(lon)
  y <- (N + elev) * cos(lat) * sin(lon)
  z <- (((b_sq/a_sq)*N) + elev) * sin(lat)
  
  return(list(x=x, y=y, z=z))
}