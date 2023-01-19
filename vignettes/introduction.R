## ----setup, include = FALSE---------------------------------------------------
library('knitr')
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library('bpmodels')

## ----echo=FALSE---------------------------------------------------------------
suppressWarnings(library('bpmodels'))
set.seed(13)

## -----------------------------------------------------------------------------
chain_sizes <- c(1, 1, 4, 7) # example of observed chain sizes
chain_ll(chain_sizes, "pois", "size", lambda = 0.5)

## ----eval=FALSE---------------------------------------------------------------
#  ?chains_ll

## -----------------------------------------------------------------------------
chain_sim(n = 5, "pois", "size", lambda = 0.5)

## -----------------------------------------------------------------------------
chain_ll(chain_sizes, "binom", "size", size = 1, prob = 0.5, nsim_offspring = 100)

## -----------------------------------------------------------------------------
ll <- chain_ll(chain_sizes, "pois", "size", obs_prob = 0.3, lambda = 0.5, nsim_obs = 10)
summary(ll)

