#!/bin/bash
## Set the version as an environment variable
VERSION="0.9.0"
ARCH_SITES="./DATA/DALPOIMGUEDES_BOCINSKY_2017.xlsx"

## Build the Docker image from the github repo
docker build -t bocinsky/asian_niche https://github.com/bocinsky/asian_niche.git#$VERSION

## Remove any previous containers
docker rm asian_niche

## Create the Docker container
docker create -w /asian_niche --name asian_niche bocinsky/asian_niche

## Start the Docker container
docker start asian_niche

## Copy the archaeological site data to the Docker container
docker cp $ARCH_SITES asian_niche:/asian_niche/DATA/DALPOIMGUEDES_BOCINSKY_2017.xlsx

## Download and copy pre-run output into the container
docker cp ~/Desktop/OUTPUT asian_niche:/asian_niche/

## Run the analysis in the docker container
docker exec asian_niche Rscript asian_niche.R

## Copy the output from the container to the host
docker cp asian_niche:/asian_niche/OUTPUT ./

## Copy README.md from the container to the host
docker cp asian_niche:/asian_niche/README.md ./README.md

## Stop the Docker container
docker stop asian_niche

## Make a Zenodo directory
rm -r Zenodo; mkdir Zenodo

## Create a compressed tar archive of the output
tar -zcf ./Zenodo/asian_niche-$VERSION-OUTPUT.tar.gz OUTPUT

## Make the Nature directory
rm -r Nature; mkdir Nature

## Copy and rename the figures, tables, and supplementary data sets for Nature
cp ./OUTPUT/FIGURES/crop_map.pdf ./Nature/Figure_1.pdf
cp ./OUTPUT/FIGURES/facet_niche.pdf ./Nature/Figure_2.pdf
cp ./OUTPUT/FIGURES/All_crossplot.pdf ./Nature/Figure_3.pdf
cp ./OUTPUT/FIGURES/All_crossplot.html ./Nature/Extended_Data_1.html
cp ./OUTPUT/FIGURES/All_wheat.mov ./Nature/Supplementary_Video_1.mov
cp ./OUTPUT/FIGURES/All_barley.mov ./Nature/Supplementary_Video_2.mov
cp ./OUTPUT/FIGURES/All_broomcorn_millet.mov ./Nature/Supplementary_Video_3.mov
cp ./OUTPUT/FIGURES/All_foxtail_millet.mov ./Nature/Supplementary_Video_4.mov
cp ./OUTPUT/FIGURES/All_buckwheat.mov ./Nature/Supplementary_Video_5.mov
cp ./DATA/crops.csv ./Nature/Supplementary_Table_1.csv
cp ./OUTPUT/TABLES/sites_dates_raw.csv ./Nature/Supplementary_Table_2.csv
cp ./OUTPUT/TABLES/age_niche_estimates.csv ./Nature/Supplementary_Table_3.csv
