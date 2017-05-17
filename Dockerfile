FROM rocker/geospatial:3.3.3

MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

# ENV GDAL_VERSION 2.2.0
# ENV GEOS_VERSION 3.6.1
# ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

RUN apt-get update

## ffmpeg
RUN apt-get install -y --no-install-recommends \
  git \
  libx264-dev \
  yasm \
  && git clone --depth 1 git://source.ffmpeg.org/ffmpeg \
  && cd ffmpeg \
  && ./configure --enable-gpl --enable-libx264 \
  && make -j4 \
  && make install \
  && cd .. \
  rm -rf ffmpeg

# ## geos
# RUN apt-get install -y --no-install-recommends \
#   lbzip2 \
#   && wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
#   && tar -xf geos-${GEOS_VERSION}.tar.bz2 \
#   && cd /geos* \
#   && ./configure \
#   && make -j4 \
#   && make install \
#   && cd .. \
#   rm -rf geos-*
# 
# ## gdal
# RUN apt-get install -y --no-install-recommends \
#     lbzip2 \
#     ## dev packages
#     libdap-dev \
#     libexpat1-dev \
#     libfftw3-dev \
#     libfreexl-dev \
#     libgsl0-dev \
#     libglu1-mesa-dev \
#     # libhdf4-alt-dev \
#     libhdf5-dev \
#     liblwgeom-dev \
#     libkml-dev \
#     libnetcdf-dev \
#     libproj-dev \
#     libsqlite3-dev \
#     libssl-dev \
#     # libtcl8.5 \
#     # libtk8.5 \
#     libtiff5-dev \
#     libudunits2-dev \
#     libxerces-c-dev \
#     netcdf-bin \
#     unixodbc-dev \
#   && wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION}.tar.gz \
#   && tar -xf gdal-${GDAL_VERSION}.tar.gz \
#   && cd /gdal* \
#   && ./configure \
#     --with-curl \
#     --with-dods-root=/usr \
#     --with-freexl \
#     --with-geos \
#     --with-geotiff \
#     --with-hdf5=/usr/lib/x86_64-linux-gnu/hdf5/serial \
#     --with-libjson-c \
#     --with-netcdf \
#     --with-odbc \
#     --without-grass \
#     --without-libgrass \
#   && make -j4 \
#   && make install \
#   && cd ..\
#   rm -rf gdal-*

## ghostscript
run apt-get install -y --no-install-recommends \
    ghostscript

## Update currently installed R packages to MRAN version
RUN r -e 'update.packages(repos="https://mran.microsoft.com/snapshot/2017-05-15", ask=FALSE)'

## Install R package dependencies from stable MRAN repo
RUN install2.r -r 'https://mran.microsoft.com/snapshot/2017-05-15' --error \
    ## Packages for Python-like command-line parsing
    devtools \
    optparse \
    ## Package for data aquisition
    FedData \
    ## Packages for parallel processing
    foreach \
    doParallel \
    ## Packages offering general utilities
    R.utils \
    Hmisc \
    zoo \
    abind \
    mgcv \
    rgbif \
    fields \
    ## Packages for spatial processing
    sf \
    rgdal \
    ncdf4 \
    raster \
    geomapdata \
    maptools \
    mapproj \
    ## Packages for chronometric analysis
    Bchron \
    mclust \
    ## Packages for tidy code
    magrittr \
    tidyverse \
    ggthemes \
    purrrlyr \
    ## Plotting
    RColorBrewer \
    htmlwidgets \
    plotly
    
## Update ggplot2 to development version (need 2.2.1.9000 for geom_sf function)
RUN r -e 'devtools::install_github("tidyverse/ggplot2")'
