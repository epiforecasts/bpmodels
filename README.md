
# *bpmodels*: Methods for analysing the size and length of chains from branching process models

<!-- badges: start -->

![CRAN/METACRAN](https://img.shields.io/cran/v/bpmodels) ![GitHub R
package
version](https://img.shields.io/github/r-package/v/epiverse-trace/bpmodels)
![GitHub all
releases](https://img.shields.io/github/downloads/epiverse-trace/bpmodels/total?style=flat)
![GitHub
issues](https://img.shields.io/github/issues/epiverse-trace/bpmodels)
[![R-CMD-check](https://github.com/epiverse-trace/bpmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/bpmodels/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/epiverse-trace/bpmodels/branch/main/graphs/badge.svg)](https://codecov.io/github/epiverse-trace/bpmodels)
![GitHub
contributors](https://img.shields.io/github/contributors/epiverse-trace/bpmodels)
![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/epiverse-trace/bpmodels)
![GitHub](https://img.shields.io/github/license/epiverse-trace/bpmodels)
<!-- badges: end -->

`bpmodels` is an R package to simulate and analyse the size and length
of branching processes with a given offspring distribution.

# Installation

The latest development version of the `bpmodels` package can be
installed via

``` r
devtools::install_github('epiverse-trace/bpmodels')
```

# Quick start

To load the package, use

At the heart of the package are the `chains_ll()` and `chains_sim()`
functions.

## Calculating log-likelihoods

The `chains_ll()` function calculates the log-likelihood of a
distribution of chain sizes or lengths given an offspring distribution
and its associated parameters.

If we have observed a distribution of chains of sizes $1, 1, 4, 7$, we
can calculate the log-likelihood of this observed chain by assuming the
offspring per generation is Poisson distributed with a mean number of
$0.5$.

To do this, we run

``` r
set.seed(13)
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
chain_ll(x = chain_sizes, offspring = "pois", stat = "size", lambda = 0.5)
#> [1] -8.607196
```

The first argument of `chain_ll()` is the size (or length) distribution
to analyse. The second argument, `offspring`, specifies the offspring
distribution. This is given as a function used to generate random
offspring. It can be any probability distribution implemented in `R`,
that is, one that has a corresponding function for generating random
numbers beginning with the letter `r`. In the case of the example above,
since random Poisson numbers are generated in `R` using a function
called `rpois()`, the string to pass to the `offspring` argument is
`"pois"`.

The third argument, `stat`, determines whether to analyse chain sizes
(`"size"`, the default if this argument is not specified) or lengths
(`"length"`). Lastly, any named arguments not recognised by `chain_ll()`
are interpreted as parameters of the corresponding probability
distribution, here `lambda = 0.5` as the mean of the Poisson
distribution (see the `R` help page for the [Poisson
distribution](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Poisson.html)
for more information).

# Imperfect observations

By default, `chain_ll` assumes perfect observation, where `obs_prob = 1`
(See `?chain_ll`). If observations are imperfect, the `chain_ll()`
function has an `obs_prob` argument that can be used to determine the
likelihood. In that case, true chain sizes or lengths are simulated
repeatedly (the number of times given by the `nsim_obs` argument), and
the likelihood calculated for each of these simulations.

For example, if the probability of observing each case is $30%$, we use

``` r
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
ll <- chain_ll(chain_sizes, "pois", "size", obs_prob = 0.3, lambda = 0.5, 
               nsim_obs = 10)
summary(ll)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  -32.09  -26.52  -24.06  -24.94  -22.49  -19.14
```

This returns `10` likelihood values (because `nsim_obs = 10`), which can
be averaged to come up with an overall likelihood estimate.

To find out about usage of the `chains_ll()` function, you can use the
`R` help file

``` r
?chains_ll
```

## Simulating branching processes

To simulate a branching process, we use the `chain_sim()` function. This
function follows the same syntax as `chain_ll()`.

Below, we are simulating $5$ chains, assuming the offspring are
generated using a Poisson distribution with mean, `lambda = 5`. By
default, `chain_sim()` returns a vector of chain sizes/lengths. However,
to override that so that a tree of infectees and infectors is returned,
we need to specify a function for the serial interval and set
`tree = TRUE`

``` r
chain_sim(n = 5, offspring = "pois", stat = "size", lambda = 0.5)
#> [1] 5 1 1 1 1
```

### Simulating trees

To simulate a tree of branching processes, we do specify the serial
interval generation function and set `tree = TRUE` as follows:

``` r
set.seed(13)

serial_interval <- function(n){rlnorm(n, meanlog = 0.58, sdlog = 1.58)}

chains_df <- chain_sim(n = 5, offspring = 'pois', lambda = 0.5, stat = 'length', 
                       infinite = 100, serial = serial_interval)

chains_df
#>    n id ancestor generation        time
#> 1  1  1       NA          1  0.00000000
#> 2  2  1       NA          1  0.00000000
#> 3  3  1       NA          1  0.00000000
#> 4  4  1       NA          1  0.00000000
#> 5  5  1       NA          1  0.00000000
#> 6  1  2        1          2  0.04771887
#> 7  5  2        1          2  5.57573333
#> 8  5  3        1          2  0.11454421
#> 9  1  3        2          3  2.64367236
#> 10 5  4        2          3  6.57843219
#> 11 1  4        3          4  2.96098160
#> 12 5  5        4          4 10.28370183
#> 13 5  6        5          5 10.37883069
```

# Methodology

If the probability distribution of chain sizes or lengths has an
analytical solution, this will be used (size distribution: Poisson and
negative binomial; length distribution: Poisson and geometric).

If an analytical solution does not exist, simulations are used to
approximate this probability distributions (using a linear approximation
to the cumulative distribution for unobserved sizes/lengths). The
argument `nsim_offspring` is used to specify the number of simulations
to be used for this approximation.

For example, to get offspring drawn from a binomial distribution with
probability `prob = 0.5`, we run

``` r
chain_ll(chain_sizes, "binom", "size", size = 1, prob = 0.5, nsim_offspring = 100)
#> [1] -8.760539
```

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
guide](https://github.com/epiverse-trace/.github/blob/main/CONTRIBUTING.md).

## Code of conduct

Please note that the *bpmodels* project is released with a [Contributor
Code of
Conduct](https://github.com/epiverse-trace/.github/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citing this package

``` r
citation("bpmodels")
#> 
#> To cite package 'bpmodels' in publications use:
#> 
#>   Funk S, Finger F (????). _bpmodels: Analysing chain statistics using
#>   branching process models_. R package version 0.1.0,
#>   <https://github.com/sbfnk/bpmodels>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {bpmodels: Analysing chain statistics using branching process models},
#>     author = {Sebastian Funk and Flavio Finger},
#>     note = {R package version 0.1.0},
#>     url = {https://github.com/sbfnk/bpmodels},
#>   }
```
