#!/bin/bash

## Set the working directory
export WKDIR=/Users/bocinsky/git/asian_niche/

## Change to $WKDIR
cd $WKDIR

## Build the Docker container
## docker build -t bocinsky/asian_niche:1.0.0 .

## Run the analysis in the docker container
docker run --rm -v $WKDIR:/asian_niche -w /asian_niche bocinsky/asian_niche:latest Rscript asian_niche.R

## Create a compressed tar archive of the output for Zenodo archiving
tar -zcf asian_niche_1.0.0_OUTPUT.tar.gz --exclude='*.DS_Store*' --exclude='*.git*' OUTPUT

## Create a compressed tar archive of the Docker container
docker save bocinsky/asian_niche:1.0.0 | gzip > asian_niche_1.0.0_DOCKER.tar.gz
