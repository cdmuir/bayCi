
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayCi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of bayCi is to …

## Installation

<!-- You can install the released version of bayCi from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("bayCi") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cdmuir/bayCi")
```

## Example

This is a basic example which shows you how to solve a common problem:

### Read LICOR data

  - determine format
  - check for errors
  - remove extraneous lines
  - find covariate columns
  - if given file name, read single file
  - if given multiple file names, read and row bind
  - if given directory, read all file names and row bind
  - set class. raw\_licor\_data

#### Single file

``` r
library(bayCi)
## basic example code
```

#### Combine multiple files

### Prepare data

## Meta

Please note that the ‘bayCi’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.