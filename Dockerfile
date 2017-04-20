# get the base image, this one has R, RStudio and pandoc
FROM rocker/verse:3.3.2

# required
MAINTAINER Kyle Bocinsky <bocinsky@gmail.com>

COPY . /OUTPUT 

 # go into the repo directory
RUN   apt-get update \
  && . /etc/environment \ 

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the vignette directory in the same structure as the package source
RUN git clone https://github.com/bocinsky/asian_niche.git  

# to build this image:
# docker build -t bocinsky/asian_niche https://raw.githubusercontent.com/bocinsky/asian_niche/master/Dockerfile

# to run this container:
# docker -dp 8787:8787 bocinsky/asian_niche
# then open broswer at localhost:8787 or http://192.168.59.103:8787/
