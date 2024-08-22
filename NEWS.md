# bpmodels 0.3.2

## Model

* `chain_sim()` now ensures that the parameters of the negative binomial offspring are not both zero else returns an informative error.

# bpmodels 0.3.1

## Unit tests and input validation

* The following internal functions now have input validation: `rborel()`, `dborel()`, `complementary_logprob()`, and `rnbinom_mean_disp()`.
* Code coverage has been improved with more tests on the following functions: `rborel()`, `dborel()`, `chain_sim()`, `rnbinom_mean_disp()`, `complementary_logprob()`, `rgen_length()`, and `rbinom_size()`.

# bpmodels 0.3.0

## Website

* The website has been updated to use bootstrap 5. This means it has a slightly
  new look.
* Exported functions under "references" have been grouped into topics.

## Vignettes

* A new vignette has been added to compile a bibliography of papers that
  apply branching processes to infectious disease epidemiology.
* The "quickstart" section of the README has been moved to the "Getting started"
  section of the website.
* The README now only provides a quick description of the core functions in the
  package and users are referred to the website for a quick start.

## README

* The README no longer has a quickstart section as it has been moved to the
  website. This is to give the README a minimalistic look.

# bpmodels 0.2.1

## Minor functionality change

* `chain_sim()` now throws a warning, instead of an error, when `tree` is set
  to `FALSE` with `serial` also specified. We assume that providing a serial
  interval means you want the tree of transmissions to be simulated,
  so `chain_sim()` internally sets `tree = TRUE` and throws a warning explaining
  what happened. This behaviour should not break any simulations with previous
  versions with `bpmodels`, but if it does, please submit an issue.
  To remove the warning, the user should explicitly set `tree = TRUE` when
  they specify `serial`.

# bpmodels 0.2.0

## Documentation

* `chain_sim()`'s help file has been updated with more details. In particular,
  we describe in detail how to specify the `serial` argument as a function. We
  have also added more examples.

* A new vignette describing how to project COVID-19 incidence with `chain_sim()`
  has been added and can be accessed on the
  [bpmodels website](https://epiverse-trace.github.io/bpmodels/) under "Articles".

* The README's "quick start" section has been updated with what was
  previously the introduction vignette.

# bpmodels 0.1.9999

* faster, vectorised chain simulations

# bpmodels 0.1.0

* initial release
