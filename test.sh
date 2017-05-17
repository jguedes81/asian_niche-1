GDAL_VERSION=2.2.0
GEOS_VERSION=3.6.1
LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

apt-get update
##  && apt-get build-dep -y -o APT::Get::Build-Dep-Automatic=true libgdal-dev \
  apt-get install -y --no-install-recommends \
    wget \
    lbzip2 \
    libhdf5-dev \
    liblwgeom-dev \
    libkml-dev \
    libnetcdf-dev \
    libproj-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxerces-c-dev \
    netcdf-bin \
    unixodbc-dev
  
  wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION}.tar.gz
  tar -xf gdal-${GDAL_VERSION}.tar.gz
  
  wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2
  tar -xf geos-${GEOS_VERSION}.tar.bz2
## Install dependencies of gdal-$GDAL_VERSION
## && echo "deb-src http://deb.debian.org/debian jessie main" >> /etc/apt/sources.list \
## Install libgeos \
  cd geos*
  ./configure
  make
  make install
## Configure options loosely based on homebrew gdal2 https://github.com/OSGeo/homebrew-osgeo4mac/blob/master/Formula/gdal2.rb
  cd gdal*
  ./configure
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
    --without-libgrass
  make
  make install
  cd ..
  ## Cleanup gdal & geos installation
  rm -rf gdal-* geos-*