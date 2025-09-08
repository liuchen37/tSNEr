
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

## Example output

![alt text](https://github.com/liuchen37/Pics/blob/main/exp_output_tsner.jpg?raw=true)

## But report and comments:

Please send an email to the author: Chen Liu at chen.liu@umontreal.ca
