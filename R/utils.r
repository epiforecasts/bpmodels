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
##' @return a sampled size
##' @author Sebastian Funk
##' @keywords internal
rbinom_size <- function(n, x, prob) {
    x + stats::rnbinom(n, x, prob) + stats::rnbinom(n, 1, prob)
}
