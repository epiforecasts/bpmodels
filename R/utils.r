#' Calculates the complementary log-probability
#'
#' Given x and norm, this calculates log(1-sum(exp(x)))
#' @param x log-probabilities
#' @return value
#' @author Sebastian Funk
#' @keywords internal
complementary_logprob <- function(x) {
    tryCatch(log1p(-sum(exp(x))), error=function(e) -Inf)
}

#' Samples size (the number of trials) of a binomial distribution
#'
#' Samples the size parameter from the binomial distribution with fixed x
#' (number of successes) and p (success probability)
#' @param n number of samples to generate
#' @param x number of successes
#' @param prob probability of success
#' @return sampled sizes
#' @author Sebastian Funk
#' @keywords internal
rbinom_size <- function(n, x, prob) {
    x + stats::rnbinom(n, x + 1, prob)
}

#' Samples chain lengths with given observation probabilities
#'
#' Samples the length of a transmission chain where each individual element is
#' observed with binomial probability with parameters n (number of successes)
#' and p (success probability)
#' @param n number of samples to generate
#' @param x observed chain lengths
#' @param prob probability of observation
#' @return sampled lengths
#' @author Sebastian Funk
#' @keywords internal
rgen_length <- function(n, x, prob) {
    x +
      ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1) +
      ceiling(log(stats::runif(n, 0, 1)) / log(1 - prob) - 1)
}

#' Finds the name of a function passed as an argument
#'
#' This works even when a function is passed multiple times (e.g., when used
#' inside an \code{\link{optim}} call).
#' See https://stackoverflow.com/a/46740314/10886760
#' @param fun function of which the name is to be determined
#' @return function name
#' @author Sebastian Funk
#' @keywords internal
find_function_name <- function(fun) {
  objects <- ls(envir = environment(fun))
  for (i in objects) {
    if (identical(fun, get(i, envir = environment(fun)))) {
      return(i)
    }
  }
}

#' Negative binomial random numbers parametrized
#' in terms of mean and dispersion coefficient
#' @param n number of samples to draw
#' @param mn mean of distribution
#' @param disp dispersion coefficient (var/mean)
#' @return vector containing the random numbers
#' @author Flavio Finger
#' @export
#' @examples
#' rnbinom_mean_disp(n = 5, mn = 4, disp = 2)
rnbinom_mean_disp <- function(n, mn, disp) {
  size <- mn / (disp - 1)
  stats::rnbinom(n, size = size, mu = mn)
  }