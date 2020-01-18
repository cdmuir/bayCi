
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

**This by no means ready to use - I've just made it public so you can see what I'm trying to do. I welcome contributions and collaboration to help move this package closer to useability.**

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

## Fit a single Rapid *A-Ci* Response (RACiR) curve

``` r
library(bayCi)
library(dplyr)
library(gunit)
library(units)
```

In this vignette we will analyze a single Rapid \(A-C_i\) Response
(RACiR) curve using the `braycir()` (pronounced like “bracer”). The
example data associted with the **bayCI** package comes from [Stinziano
*et al.* (2017)](https://doi.org/10.1111/pce.12911). It contains RACiR
curves from *Poplar deltoides* and the object is called
`stinziano_etal_2017`. It can accessed once **bayCI** is loaded into
your environment.

### Step 1: Convert data to the `racir` class

The `braycir()` function is for fitting a single RACiR curve. The `data`
argument in the `braycir()` function must inherit class `racir`. This
ensures that all required variables:

  - are present in the data
  - have the correct column name
  - have proper scientific units

The `racir()` function will make a new `racir` object from a
`data.frame` and validate it.

``` r

df <- stinziano_etal_2017 %>%

  # The dataset contains multiple RACiR curves, so we will filter out all but one
  filter(curve_type == "RACiR 0 to 500", plant_id == "A") %>%

  # Set units
  mutate(
    Ci    = set_units( Ci,    umol / mol     ),
    A     = set_units( A,     umol / m^2 / s ),
    Tleaf = set_units( Tleaf, degreeC        ),
    PARi  = set_units( PARi,  umol / m^2 / s ),
    E     = set_units( E,     mol / m^2 / s  ),
    gsc   = set_units( gsc,   mol / m^2 / s  ),
    Cs    = set_units( Cs,    umol / mol     ),
    Cr    = set_units( Cr,    umol / mol     ),
    Pa    = set_units( Pa,    kPa            )
  ) %>%
  
  # Following Sharkey et al. (2007), bayCi uses Pa units
  mutate(
    Pci = set_units(Ci * Pa, Pa),
    Pgsw = convert_conductance(gsc, P = Pa, Temp = Tleaf)$`umol/m^2/s/Pa`
  )

rcr <- racir(df)

class(rcr)
#> [1] "tbl"        "tbl_df"     "racir"      "data.frame"

rcr %>%
  select(A, Ci) %>%
  mutate_if(is.numeric, signif, digits = 3) %>%
  head() %>%
  knitr::kable()
```

|                   A |               Ci |
| ------------------: | ---------------: |
| 6.33 \[umol/m^2/s\] | 107 \[umol/mol\] |
| 6.48 \[umol/m^2/s\] | 109 \[umol/mol\] |
| 6.74 \[umol/m^2/s\] | 110 \[umol/mol\] |
| 6.76 \[umol/m^2/s\] | 112 \[umol/mol\] |
| 7.04 \[umol/m^2/s\] | 113 \[umol/mol\] |
| 7.15 \[umol/m^2/s\] | 115 \[umol/mol\] |

### Step 2: Convert empty chamber RACiR correction data to the `empty` class

Raw RACiR data be corrected using a “blank” curve measured on an empty
chamber. These data are passed to `braycir()` using the `empty`, which
must inherit class `empty`. This performs the same set of checks as the
`racir` class. The `empty()` function will make a new `empty` object
from a `data.frame` and validate it. The correction curve from Stinziano
*et al.* (2017) comes with the **bayCi** package and can be accessed
from `stinziano_etal_2017_empty`.

``` r

df <- stinziano_etal_2017_empty %>%
  
  # Set units
  mutate(
    A     = set_units( A,     umol / m^2 / s ),
    Ci    = set_units( Ci,    umol / mol     ),
    Pa    = set_units( Pa,    kPa            ),
    Tleaf = set_units( Tleaf, degreeC        )
  ) %>%
  
  # Following Sharkey et al. (2007), bayCi uses Pa units
  mutate(
    Pci = set_units(Ci * Pa, Pa)
  )

mty <- empty(df)

class(mty)
#> [1] "tbl"        "tbl_df"     "empty"      "data.frame"

mty %>%
  mutate_if(is.numeric, signif, digits = 3) %>%
  head() %>%
  knitr::kable()
```

|               Ci |                      A |     Tleaf |           Pa |         Pci |
| ---------------: | ---------------------: | --------: | -----------: | ----------: |
| 501 \[umol/mol\] | \-0.718 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |
| 501 \[umol/mol\] | \-0.641 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |
| 501 \[umol/mol\] | \-0.629 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |
| 501 \[umol/mol\] | \-0.728 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |
| 501 \[umol/mol\] | \-1.430 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |
| 501 \[umol/mol\] | \-2.690 \[umol/m^2/s\] | 25 \[°C\] | 84.1 \[kPa\] | 42.1 \[Pa\] |

### References

Sharkey TD, CJ Bernacchi, GD Farquhar, EL Singsaas. 2007. [Fitting
photosynthetic carbon dioxide response curves for C\(_3\)
leaves](https://doi.org/10.1111/j.1365-3040.2007.01710.x). *Plant, Cell
& Environment* 30(9): 1035–1040.

Stinziano JR, PB Morgan, DJ Lynch, AJ Saathoff, DK McDermitt, DT Hanson.
2017. [The rapid \(A-C_i\) response: photosynthesis in the phenomic
era](https://doi.org/10.1111/pce.12911). *Plant, Cell & Environment*
40(8): 1256–1262.

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
