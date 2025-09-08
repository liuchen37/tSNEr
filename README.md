
# tSNEr

<!-- badges: start -->
<!-- badges: end -->

The goal of tSNEr is to perform and visualise t-Distributed Stochastic Neighbour Embedding based on user-provided flow cytometry FCS files. This package does not require an internet connection.

## Installation

You can install the development version of tSNEr like so:

``` r
# Install from your GitHub repository
devtools::install_github("liuchen37/tSNEr")

# Load the package
library(tSNEr)
```

## Usage

Running tSNEr is easy. Put your .fcs files in the folder where the R project was created, then execute:

``` r
library(tSNEr)
result <- main()
```

