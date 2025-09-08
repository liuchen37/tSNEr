
# tSNEr

![alt text](https://github.com/liuchen37/Pics/blob/main/tSNEr_logo_small.png?raw=true)

<!-- badges: start -->
<!-- badges: end -->

The goal of tSNEr is to perform and visualise t-Distributed Stochastic Neighbour Embedding based on user-provided flow cytometry FCS files. This package does not require an internet connection.

## Installation

You can install the development version of tSNEr like so:

``` r
# Install from your GitHub repository
devtools::install_github("liuchen37/tSNEr")
```

## Prerequisites:

[base64enc](https://cran.r-project.org/web/packages/base64enc/index.html), [Rtsne](https://github.com/jkrijthe/Rtsne), [dplyr](https://github.com/tidyverse/dplyr), [tidyr](https://github.com/tidyverse/tidyr), [flowCore](https://github.com/RGLab/flowCore), [ggplot2](https://github.com/tidyverse/ggplot2), [gridExtra](https://github.com/sourcechord/GridExtra), [htmltools](https://github.com/rstudio/htmltools) and [RColorBrewer](https://github.com/cran/RColorBrewer).

To install and load all requested packages:

``` r
cran_packages <- c("Rtsne", "ggplot2", "dplyr", "tidyr", "gridExtra", "htmltools", "RColorBrewer", "base64enc")
bioc_packages <- c("flowCore")

installed_packages <- rownames(installed.packages())
for (pkg in cran_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install Bioconductor packages
for (pkg in bioc_packages) {
  if (!pkg %in% installed_packages) {
    BiocManager::install(pkg)
  }
}

# Load all packages
lapply(c(cran_packages, bioc_packages), library, character.only = TRUE)
```

## Usage

Running tSNEr is easy. Put your .fcs files in the folder where the R project was created, then execute:

``` r
library(tSNEr)
result <- main()
```

## Example output

![alt text](https://github.com/liuchen37/Pics/blob/main/exp_output_tsner.jpg?raw=true)

## But report and comments:

Please send an email to the author: Chen Liu at chen.liu@umontreal.ca
