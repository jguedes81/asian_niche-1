#!/bin/bash
## RUN THIS FROM WITHIN THE `asian_niche` DIRECTORY

## Build the Docker container
docker build -t bocinsky/asian_niche .

## Copy the archaeological site data to the Docker container
docker cp ./asian_niche/DATA/DALPOIMGUEDES_BOCINSKY_2017.xlsx bocinsky/asian_niche:/asian_niche/DATA/DALPOIMGUEDES_BOCINSKY_2017.xlsx

## Run the analysis in the docker container
docker run -w /asian_niche bocinsky/asian_niche Rscript asian_niche.R

## Copy the output from the container to the host
docker cp bocinsky/asian_niche:/asian_niche/OUTPUT ./OUTPUT

## Remove the archaeological site data from the Docker container
docker run -w /asian_niche bocinsky/asian_niche rm ./DATA/DALPOIMGUEDES_BOCINSKY_2017.xlsx

## Make a Zenodo directory
mkdir Zenodo

## Create a compressed tar archive of the output
tar -zcf asian_niche_1.0.0_OUTPUT.tar.gz OUTPUT

## Create a compressed tar archive of the Docker container
docker save bocinsky/asian_niche | gzip > asian_niche_1.0.0_DOCKER.tar.gz

## Make the Nature directory
mkdir Nature

## Copy and rename the figures, tables, and supplementary data sets for Nature
cp ./OUTPUT/FIGURES/crop_map.pdf ./Nature/Figure_1.pdf
cp ./OUTPUT/FIGURES/facet_niche.pdf ./Nature/Figure_2.pdf
cp ./OUTPUT/FIGURES/All_crossplot.pdf ./Nature/Figure_3.pdf
cp ./OUTPUT/FIGURES/All_crossplot.html ./Nature/Extended_Data_1.html
cp ./OUTPUT/FIGURES/All_wheat.mov ./Nature/Supplementary_Video_1.mov
cp ./OUTPUT/FIGURES/All_barley.mov ./Nature/Supplementary_Video_2.mov
cp ./OUTPUT/FIGURES/All_broomcorn_millet.mov ./Nature/Supplementary_Video_3.mov
cp ./OUTPUT/FIGURES/All_foxtail_millet.mov ./Nature/Supplementary_Video_4.mov
