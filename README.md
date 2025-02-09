
# *bpmodels*: Methods for simulating and analysing the size and length of transmission chains from branching process models

<!-- badges: start -->

[![Lifecycle:
retired](https://img.shields.io/badge/lifecycle-retired-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#retired)
![GitHub R package
version](https://img.shields.io/github/r-package/v/epiforecasts/bpmodels)
[![R-CMD-check](https://github.com/epiforecasts/bpmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiforecasts/bpmodels/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiforecasts/bpmodels/branch/main/graph/badge.svg)](https://app.codecov.io/github/epiforecasts/bpmodels)
![GitHub
contributors](https://img.shields.io/github/contributors/epiforecasts/bpmodels)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/MIT/)
<!-- badges: end -->

## WARNING

> `{bpmodels}` is now *retired and will no longer be maintained*. We
> recommend using
> [`{epichains}`](https://github.com/epiverse-trace/epichains) instead.
> If you need help converting your code to use `{epichains}`, please
> [open a discussion on
> epichains](https://github.com/epiverse-trace/epichains/discussions).

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
pak::pkg_install("epiforecasts/bpmodels")
```

To load the package, use

``` r
library("bpmodels")
#> Note: bpmodels is now retired and replaced by epichains. All features from bpmodels are available in epichains. Get epichains from <https://github.com/epiverse-trace/epichains>.Thank you for your support!
```

# Core functionality

*bpmodels* provides three main functions:

`chain_ll()`: calculates the likelihoods of observing a vector of chains
of given sizes or lengths.

Here is a quick example of estimating the loglikelihood of an observed
chain:

``` r
# example of observed chain sizes
chain_sizes <- c(1, 2, 3, 4) 
# estimate loglikelihood of the observed chain sizes
chain_ll_eg <- chain_ll(x = chain_sizes, offspring = "pois", 
                        stat = "size", lambda = 0.5)
chain_ll_eg
#> [1] -7.772589
```

`chain_sim()`: simulates transmission chains until all chains stop
producing offspring.

Below is a quick example where we simulate the chain sizes of $5$ chains
with a poisson offspring with mean, $\text{lambda} = 0.5$:

``` r
set.seed(123)

chain_sim_eg <- chain_sim(n = 5, offspring = "pois", stat = "size", 
                          lambda = 0.5, tree = TRUE)

head(chain_sim_eg)
#>   n id ancestor generation
#> 1 1  1       NA          1
#> 2 2  1       NA          1
#> 3 3  1       NA          1
#> 4 4  1       NA          1
#> 5 5  1       NA          1
#> 6 2  2        1          2
```

`chain_sim_susc()`: simulates transmission chains from a specified
population size with pre-existing immunity until the susceptible pool
runs out.

Below is a quick example where we simulate chains with a poisson
offspring with mean, $\text{lambda} = 0.5$, and serial interval of $3$:

``` r
set.seed(1234)

chain_sim_susc_eg <- chain_sim_susc(pop = 1000, offspring = "pois",
                                    mn_offspring = 0.5,
                                    serial = function(x) {3}
                                    )

head(chain_sim_susc_eg)
#>   id ancestor generation time
#> 1  1       NA          1    0
```

See the [“Get started
vignette”](https://epiforecasts.github.io/bpmodels/articles/bpmodels.html)
for a detailed illustration of each function.

## Package vignettes

Specific use cases of *bpmodels* can be found in the [online
documentation as package
vignettes](https://epiforecasts.github.io/bpmodels/), under “Articles”.

## Reporting bugs

To report a bug please open an
[issue](https://github.com/epiforecasts/bpmodels/issues/new/choose).

## Contribute

We welcome contributions to enhance the package’s functionalities. If
you wish to do so, please follow the [package contributing
guide](https://github.com/epiforecasts/bpmodels/blob/main/.github/CONTRIBUTING.md).

## Code of conduct

Please note that the *bpmodels* project is released with a [Contributor
Code of
Conduct](https://github.com/epiforecasts/.github/blob/main/CODE_OF_CONDUCT.md).
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
