# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse:3.3.3

# required
MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

ENV GDAL_VERSION 2.2.0
ENV GEOS_VERSION 3.6.1
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

RUN apt-get update \
##  && apt-get build-dep -y -o APT::Get::Build-Dep-Automatic=true libgdal-dev \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    ## dev packages
    # libdap-dev \
    # libexpat1-dev \
    # libfftw3-dev \
    # libfreexl-dev \
#  libgeos-dev \ # Need 3.5, this is 3.3
    # libgsl0-dev \
    # libglu1-mesa-dev \
    # libhdf4-alt-dev \
    libhdf5-dev \
    liblwgeom-dev \
    libkml-dev \
    libnetcdf-dev \
    libproj-dev \
    # libsqlite3-dev \
    libssl-dev \
    # libtcl8.5 \
    # libtk8.5 \
    libtiff5-dev \
    libudunits2-dev \
    libxerces-c-dev \
   ## runtime packages
    netcdf-bin \
    unixodbc-dev \
  && wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION}.tar.gz \
  && tar -xf gdal-${GDAL_VERSION}.tar.gz \
  && wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
  && tar -xf geos-${GEOS_VERSION}.tar.bz2 \
## Install dependencies of gdal-$GDAL_VERSION
## && echo "deb-src http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
## Install libgeos \
  && cd /geos* \
  && ./configure \
  && make \
  && make install \
## Configure options loosely based on homebrew gdal2 https://github.com/OSGeo/homebrew-osgeo4mac/blob/master/Formula/gdal2.rb
  && cd /gdal* \
  && ./configure \
    --with-curl \
    --with-dods-root=/usr \
    --with-freexl \
    --with-geos \
    --with-geotiff \
    # --with-hdf4 \
    --with-hdf5=/usr/lib/x86_64-linux-gnu/hdf5/serial \
    --with-libjson-c \
    --with-netcdf \
    --with-odbc \
    ##
    --without-grass \
    --without-libgrass \
  && make \
  && make install \
  && cd .. \
  ## Cleanup gdal & geos installation
  && rm -rf gdal-* geos-* # \
  
# ## Install R packages labeled "core" in Spatial taskview 
#   && install2.r -r 'https://mran.microsoft.com/snapshot/2017-05-01' --error \
#     ## from CRAN
#     DCluster \
#     RColorBrewer \
#     RandomFields \
#     classInt \
#     deldir \
#     dggridR \
#     gstat \
#     lidR \
#     maptools \
#     ncdf4 \
#     proj4 \
#     raster \
#     rgdal \
#     rgeos \
#     rlas \
#     sf \
#     sp \
#     spacetime \
#     spatstat \
#     spdep \
#     splancs \
#     geoR \
#     ## from bioconductor
#     && R -e "BiocInstaller::biocLite('rhdf5')"
#     




## get the base image, this one has R, RStudio and pandoc
#FROM rocker/verse
#
## required
#MAINTAINER Ben Marwick <benmarwick@gmail.com>
#
## stay current
#RUN apt-get update -y \
#
#  # solve a mysterious & sudden error with XML pkg
#  && apt-get install libxml2-dev -y \
#
#
#  # get the full set of repository files from GitHub
#  && git clone https://github.com/benmarwick/researchcompendium.git \
#  # make these files writable
#  && chmod 777 -R researchcompendium \
#  # go into the repo directory
#  && cd /researchcompendium \
#  # start R and build pkgs that we depend on from
#  # local sources that we have collected with packrat
#  && R -e "0" --args --bootstrap-packrat \
#  # build this compendium package
#  && R -e 'devtools::install(".")' \
#  # render the manuscript into a docx
#  && R -e "rmarkdown::render('analysis/paper/paper.Rmd')"


#################### Notes to self ###############################
# a suitable disposable test env:
# docker run -dp 8787:8787 rocker/rstudio

# to build this image:
# docker build -t benmarwick/researchcompendium https://raw.githubusercontent.com/benmarwick/researchcompendium/master/Dockerfile

# to run this container to work on the project:
# docker run -dp 8787:8787  -v /c/Users/bmarwick/docker:/home/rstudio/ -e ROOT=TRUE  benmarwick/researchcompendium
# then open broswer at localhost:8787 or run `docker-machine ip default` in the shell to find the correct IP address

# go to hub.docker.com
# create empty repo for this repo ('Create Automated Build'), then

# to add CI for the docker image
# add .circle.yml file
# - Pushes new image to hub on successful complete of test
# - And gives a badge to indicate test status
# go to circle-ci to switch on this repo

# On https://circleci.com/gh/benmarwick/this_repo
# I need to set Environment Variables:
# DOCKER_EMAIL
# DOCKER_PASS
# DOCKER_USER

# Circle will push to docker hub automatically after each commit, but
# to manually update the container at the end of a work session:
# docker login # to authenticate with hub
# docker push benmarwick/researchcompendium

# When running this container, the researchcompendium dir is not writable, so we need to
# sudo chmod 777 -R researchcompendium

#
