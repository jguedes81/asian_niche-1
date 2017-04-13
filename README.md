<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2017--04--13-brightgreen.svg)](https://github.com/bocinsky/asian_niche/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.3-brightgreen.svg)](https://cran.r-project.org/) [![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![Zenodo DOI](https://zenodo.org/badge/23774237.svg)](https://zenodo.org/badge/latestdoi/23774237)

Research compendium for a report on xxxx
----------------------------------------

### Compendium DOI:

<http://dx.doi.org/xxxxxxx>

The files at the URL above will generate the results as found in the publication. The files hosted at github.com/benmarwick/researchcompendium are the development versions and may have changed since the report was published

### Authors of this repository:

Kyle Bocinsky (<bocinsky@gmail.com>)

### Published in:

Bocinsky, R. Kyle, xxxxx

### Overview of contents

This repository is our research compendium for our analysis of xxxx. The compendium contains all data, code, and text associated with the publication. The `Rmd` files in the `analysis/paper/` directory contain details of how all the analyses reported in the paper were conducted, as well as instructions on how to rerun the analysis to reproduce the results. The `data/` directory in the `analysis/` directory contains all the raw data.

### The supplementary files

The `analysis/` directory contains:

-   the manuscript as submitted (in MS Word format) and it's Rmd source file
-   all the data files (in CSV format, in the `data/` directory)
-   supplementary information source files (in R markdown format) and executed versions
-   all the figures that are included in the paper (in the `figures/` directory)

### The R package

This repository is organized as an R package. There are no actual R functions in this package - all the R code is in the Rmd file. I simply used the R package structure to help manage dependencies, to take advantage of continuous integration for automated code testing, and so I didn't have to think too much about how to organise the files.

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt (assuming you have Git installed on your computer):

``` bash
git clone https://github.com/bocinsky/asian_niche.git
```

Once the download is complete, open the `asian_niche.Rproj` in RStudio to begin working with the package and compendium files.

The package has a number of dependencies on other R packages, and programs outside of R. These are listed at the bottom of this README. Installing these can be time-consuming and complicated, so we've done two things to simpify access to the compendium. First is the packrat directory, which contains the source code for all the packages we depend on. If all works well, these will be installed on your computer when you open `researchcompendium.Rproj` in RStudio. Second is our Docker image that includes all the necessary software, code and data to run our analysis. The Docker image may give a quicker entry point to the project, and is more self-contained, so might save some fiddling with installing things.

### Licenses

Manuscript: [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: [MIT](http://opensource.org/licenses/MIT) year: 2017, copyright holder: R. Kyle Bocinsky and Jade d'Alpoim Guedes

Data: [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse

### Dependencies

I used [RStudio](http://www.rstudio.com/products/rstudio/) on Ubuntu 16.04 and Windows 7. See the colophon section of the docx file in `analysis/paper` for a full list of the packages that this project depends on.

### Contact

Kyle Bocinsky Research Associate Crow Canyon Archaeological Center 23390 Road K, Cortez, CO 81321 970.564.4384 – Office 770.362.6659 – Mobile <bocinsky@gmail.com> – Email bocinsky.io – Web
