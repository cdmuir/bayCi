
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayCi

<!-- badges: start -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/bayCi)](https://cran.r-project.org/package=bayCi) -->

[![Build
Status](https://travis-ci.org/cdmuir/bayci.svg?branch=master)](https://travis-ci.org/cdmuir/bayCi)
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2808079.svg)](https://doi.org/10.5281/zenodo.2808079) -->
<!-- [![](https://cranlogs.r-pkg.org/badges/bayCi)](https://cran.r-project.org/package=bayCi) -->
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

## Contributors

  - [Chris Muir](https://github.com/cdmuir)

## Comments and contributions

I welcome comments, criticisms, and especially contributions\! GitHub
issues are the preferred way to report bugs, ask questions, or request
new features. You can submit issues here:

<https://github.com/cdmuir/bayCi/issues>

## Meta

  - Please [report any issues or
    bugs](https://github.com/cdmuir/bayCi/issues).
  - License: MIT
    <!-- * Get citation information for `bayCi` in R doing `citation(package = 'bayCi')` -->
  - Please note that the ‘bayCi’ project is released with a [Contributor
    Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
    project, you agree to abide by its terms.
