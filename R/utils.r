##' Calculates the complementary log-probability
##'
##' Given x and norm, this calculates log(1-sum(exp(x)))
##' @param x log-probabilities
##' @return value
##' @author Sebastian Funk
##' @keywords internal
complementary_logprob <- function(x) {
    tryCatch(log1p(-sum(exp(x))), error=function(e) -Inf)
}

##' Samples size (the number of trials) of a binomial distribution
##'
##' Samples the size parameter from the binomial distribution with fixed x
##' (number of successes) and p (success probability)
##' @param n number of samples to generate
##' @param x number of successes
##' @param prob probability of success
##' @return sampled sizes
##' @author Sebastian Funk
##' @keywords internal
rbinom_size <- function(n, x, prob) {
    x + stats::rnbinom(n, x + 1, prob)
}

##' Samples chain lengths with given observation probabilities
##'
##' Samples the length of a transmission chain where each individual element is
##' observed with binomial probability (number of successes) and p (success
##' probability)
##' @param n number of samples to generate
##' @param x observed chain lengths
##' @param prob probability of observation
##' @return sampled lengths
##' @author Sebastian Funk
##' @keywords internal
rgen_length <- function(n, x, prob) {
    x +
      ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1) +
      ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1)
}
