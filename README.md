<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2017--05--17-brightgreen.svg)](https://github.com/bocinsky/asian_niche/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-brightgreen.svg)](https://cran.r-project.org/) [![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) <!-- [![Zenodo DOI](https://zenodo.org/badge/23774237.svg)](https://zenodo.org/badge/latestdoi/23774237) -->

Research compendium for d'Alpoim Guedes and Bocinsky *in review*
----------------------------------------------------------------

d'Alpoim Guedes, Jade and R. Kyle Bocinsky. Climate change stimulated agricultural innovation and exchange across Asia. Submitted to *Nature*.

### Compendium DOI:

<!-- http://dx.doi.org/xxxxxxx -->
The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/bocinsky/asian_niche> are the development versions and may have changed since this compendium was released.

### Authors of this repository:

R. Kyle Bocinsky (<bocinsky@gmail.com>) Jade d'Alpoim Guedes

### Overview of contents

This repository is a research compendium for d'Alpoim Guedes and Bocinsky (in review). The compendium contains all code associated with the analyses described and presented in the publication, as well as a Docker environment (described in the `Dockerfile`) for running the code. The `asian_niche.R` script runs the entire analysis, and is designed to be called from the command prompt. `asian_niche.srun` is the configuration script for the [Slurm Workload Manager](https://slurm.schedmd.com/) that was used to run this analysis on the [Kamiak high performance computing cluster](https://hpc.wsu.edu/) at Washington State University---but the `asian_niche.R` script can be run on its own. The `src/` directory contains R code for functions used in the `asian_niche.R` script.

### The research compendium

To download this research compendium as you see it on GitHub, for offline browsing, [install git on your computer](https://git-scm.com/) and use this line at the shell prompt:

``` bash
git clone https://github.com/bocinsky/asian_niche.git
```

### Archaeological site data

### The Docker container

[Docker](https://www.docker.com/) is a virtual computing environment that facilitates reproducible research---it allows for research results to be produced independent of the machine on which they are computed. Docker users describe computing environments in a text format called a "Dockerfile", which when read by the Docker software builds a virtual machine, or "container". Other users can then load the container on their own computers. Users can upload container images to [Docker Hub](https://hub.docker.com/), and the image for this research is available at <https://hub.docker.com/r/bocinsky/asian_niche/>.

We have included a Dockerfile which builds a Docker container for running the analyses described in the paper. It uses [`rocker/verse:3.4.0`](https://hub.docker.com/r/rocker/verse/) (which provides R, [RStudio Server](https://www.rstudio.com/products/rstudio/download-server/), and the [tidyverse](http://tidyverse.org/)) as its base image and adds several geospatial software packages ([GDAL](http://www.gdal.org/), [GEOS](https://trac.osgeo.org/geos/), and [proj.4](http://proj4.org/)), as well as the R software packages required by the script.

#### Downloading and running the Docker container image

``` bash
docker run -v /Users/bocinsky/git/asian_niche:/asian_niche -dp 8787:8787 bocinsky/asian_niche:latest

docker run -v /Users/bocinsky/git/asian_niche:/asian_niche -w /asian_niche -it bocinsky/asian_niche:latest bash

docker run -v /Users/bocinsky/git/asian_niche:/asian_niche -w /asian_niche bocinsky/asian_niche:latest Rscript --vanilla asian_niche.R
```

#### Building the Docker container

If you wish to build the Docker container for this project from scratch, simply `cd` into the `asian_niche/` directory and run:

``` bash
docker build -t asian_niche - < Dockerfile
```

The `-t` argument gives the resulting container image a name, and the `- < Dockerfile` argument instructs Docker to build independently of the

### Opening and running the RStudio project

This repository is organized as an [RStudio](https://www.rstudio.com/) project---it is best viewed using the RStudio integrated development environment for R. Begin by [installing RStudio](https://www.rstudio.com/products/rstudio/download2/), and then open the `asian_niche.Rproj` file in the downloaded directory.

### Running the script from the command prompt

Alternatively, the `asian_niche.R` script has been designed to be run from the terminal using Python-style argument parsing---see an example in the `asian_niche.srun` file. To run, simply use this line at the shell prompt:

``` bash
Rscript --vanilla asian_niche.R
```

Passing the `--vanilla` option runs the script in a "fresh" R environment. Run `Rscript asian_niche.R --help` to see all available options.

### Output

The `OUTPUT/` directory contains all data generated by the `asian_niche.R` script:

-   `session_info.txt` describes the computational infrastructure within which the script was run
-   `DATA/` contains data downloaded from web sources for this analysis
-   `MODELS/` contains R data objects describing the Kriging interpolation models across the study area
-   `RECONS/` contains NetCDF format raster bricks of the model output (i.e., the reconstructed crop niches)
-   `SITE_DENSITIES/` contains figures of the estimated chronometric probability density for each site in our database
-   `FIGURES/` contains all figures output by the script, including videos of how each crop niche changes over time
-   `TABLES/` contains tables of the raw site chronometric data without locational information, and the modeled chronometric probability and niche information for each site

### The R package

This repository is organized as an R package. There are no actual R functions in this package - all the R code is in the Rmd file. I simply used the R package structure to help manage dependencies, to take advantage of continuous integration for automated code testing, and so I didn't have to think too much about how to organise the files.

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt (assuming you have Git installed on your computer):

``` bash
git clone https://github.com/bocinsky/asian_niche.git
```

Once the download is complete, open the `asian_niche.Rproj` in RStudio to begin working with the package and compendium files.

The package has a number of dependencies on other R packages, and programs outside of R. These are listed at the bottom of this README. Installing these can be time-consuming and complicated, so we've done two things to simpify access to the compendium. First is the packrat directory, which contains the source code for all the packages we depend on. If all works well, these will be installed on your computer when you open `asian_niche.Rproj` in RStudio. Second is our Docker image that includes all the necessary software, code and data to run our analysis. The Docker image may give a quicker entry point to the project, and is more self-contained, so might save some fiddling with installing things.

### Licenses

Code: [MIT](http://opensource.org/licenses/MIT) year: 2017, copyright holder: R. Kyle Bocinsky and Jade d'Alpoim Guedes

### Contact

Kyle Bocinsky Research Associate Crow Canyon Archaeological Center 23390 Road K, Cortez, CO 81321 970.564.4384 – Office 770.362.6659 – Mobile <bocinsky@gmail.com> – Email bocinsky.io – Web
