
# *bpmodels*: Methods for analysing the size and length of transmission chains from branching process models

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/epiverse-trace/bpmodels)
[![R-CMD-check](https://github.com/epiverse-trace/bpmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/bpmodels/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiverse-trace/bpmodels/branch/main/graphs/badge.svg)](https://codecov.io/github/epiverse-trace/bpmodels)
![GitHub
contributors](https://img.shields.io/github/contributors/epiverse-trace/bpmodels)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

*bpmodels* is an R package to simulate and analyse the size and length
of branching processes with a given offspring distribution. These models
are often used in infectious disease epidemiology, where the chains
represent chains of transmission, and the offspring distribution
represents the distribution of secondary infections caused by an
infected individual.

# Installation

The latest development version of the *bpmodels* package can be
installed via

``` r
# check whether {pak} is installed
if (!require("pak")) install.packages("pak")
pak::pkg_install("epiverse-trace/bpmodels")
```

To load the package, use

``` r
library("bpmodels")
```

# Core functionality

*bpmodels* provides three functions:

- `chain_ll()`: calculates the likelihoods of observing a vector of
  chains of given sizes or lengths.

- `chain_sim()`: simulates transmission chains until all chains die out.

- `chain_sim_susc()`: simulates transmission chains until a specified
  initial susceptible pool runs out.

See the [“introduction vignette”](bpmodels) for a detailed illustration
of each function.

## Package vignettes

Specific use cases of *bpmodels* can be found in the [online
documentation as package
vignettes](https://epiverse-trace.github.io/bpmodels/), under
“Articles”.

## Reporting bugs

To report a bug please open an
[issue](https://github.com/epiverse-trace/bpmodels/issues/new/choose).

## Contribute

We welcome contributions to enhance the package’s functionalities. If
you wish to do so, please follow the [package contributing
guide](https://github.com/epiverse-trace/bpmodels/blob/main/.github/CONTRIBUTING.md).

## Code of conduct

Please note that the *bpmodels* project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citing this package

``` r
citation("bpmodels")
#> To cite package bpmodels in publications use:
#> 
#>   Sebastian Funk, Flavio Finger, and James M. Azam (2023). bpmodels:
#>   Analysing transmission chain statistics using branching process
#>   models, website: https://github.com/epiverse-trace/bpmodels/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {bpmodels: Analysing transmission chain statistics using branching process models},
#>     author = {{Sebastian Funk} and {Flavio Finger} and {James M. Azam}},
#>     year = {2023},
#>     url = {https://github.com/epiverse-trace/bpmodels/},
#>   }
```
