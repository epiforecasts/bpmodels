[bpmodels](https://github.com/sbfnk/bpmodels) is an `R` package to
analyse and simulate the size and length of branching processes with a
given offspring distribution. These can be used, for example, to analyse
the distribution of chain sizes or length of infectious disease
outbreaks.

Usage
=====

To load the package, use

    library('bpmodels')

At the heart of the package are the `chains_ll` and `chains_sim`
functions. The `chains_ll` function calculates the log-likelihood of a
distribution of chain sizes or lengths given an offspring distribution
and associated parameters. For example, to get the log-likelihood for a
given observed distribution of chain sizes assuming a mean number of 0.5
Poisson-distributed offspring per generation, use

    chain_sizes <- c(1,1,4,7) # example of observed chain sizes
    chain_ll(chain_sizes, "pois", "size", lambda=0.5)
    #> [1] -8.607196

The first argument of `chain_ll` is the size (or length) distribution to
analyse. The second argument (called `offspring`) specifies the
offspring distribution. This is given as a the function used to generate
random offspring. It can be any probability distribution implemented in
R, that is, one that has a corresponding function for generating random
numbers beginning with the letter `r`. In the case of the example above,
since random Poisson numbers are generated in R using a function called
`rpois`, the string to pass to the `offspring` argument is `"pois"`.

The third argument (called `stat`) determines whether to analyse chain
sizes (`"size"`, the default if this argument is not specified) or
lengths (`"length"`). Lastly, any named arguments not recognised by
`chain_ll` are interpreted as parameters of the corresponding
probability distribution, here `lambda=0.5` as the mean of the Poisson
distribution (see the R help page for the Poisson distribution for more
information).

You can use the `R` help to find out about usage of the `chains_ll`
function,

    ?chains_ll

To simulate from a branching process, use the `chain_sim` function,
which follows the same syntax as the `chain_ll` function:

    chain_sim(n=5, "pois", "size", lambda=0.5)
    #> [1] 2 1 1 1 5

Methodology
===========

If the probability distribution of chain sizes or lengths has an
analytical solution, this will be used (size distribution: Poisson and
negative binomial; length distribution: Poisson and geometric). If not,
simulations are used to approximate this probability distributions
(using a linear approximation to the cumulative distribution for
unobserved sizes/lengths), requiring an additional parameter
`nsim_offspring` for the number of simulations to be used for this
approximation. For example, to get offspring drawn from a binomial
distribution with probability `prob=0.5`.

    chain_ll(chain_sizes, "binom", "size", size=1, prob=0.5, nsim_offspring=100)
    #> [1] -8.477588

Imperfect observations
======================

The `chain_ll` function has an `obs_prob` parameter that can be used to
determine the likelihood if observations are imperfect. In that case,
true chain sizes or lengths are simulated repeatedly (the number of
times given by the `nsim_obs` argument) and the likelihood calculated
for each of these simulations. For example, if the probability of
observing each case is 30%, use

    ll <- chain_ll(chain_sizes, "pois", "size", obs_prob = 0.3, lambda=0.5, nsim_obs=10)
    summary(ll)
    #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #>  -35.30  -25.68  -23.23  -24.19  -20.89  -18.91

This returns `nsim_obs=10` likelihood values which can be averaged to
come up with an overall likelihood estimate.

References
==========

-   Farrington, C.P., Kanaan, M.N. and Gay, N.J. (2003). [Branching
    process models for surveillance of infectious diseases controlled by
    mass vaccination](https://doi.org/10.1093/biostatistics/4.2.279).
-   Blumberg, S. and Lloyd-Smith, J.O. (2013). [Comparing methods for
    estimating R0 from the size distribution of subcritical transmission
    chains](https://doi.org/10.1016/j.epidem.2013.05.002).
