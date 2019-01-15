##' Likelihood of the size of chains with Poisson offspring distribution
##'
##' @param x vector of sizes
##' @param lambda rate of the Poisson distributino
##' @return log-likelihood values
##' @author Sebastian Funk
pois_size_ll <- function(x, lambda)
{
  (x - 1) * log(lambda) - lambda * x + (x - 2) * log(x) - lgamma(x)
}

##' Likelihood of the size of chains with Negative-Binomial offspring distribution
##'
##' @param x vector of sizes
##' @param size the dispersion parameter (often called \code{k} in ecological applications)
##' @param prob probability of success (in the parameterisation with \code{prob}, see also \code{\link[stats]{NegBinomial}})
##' @param mu mean parameter
##' @return log-likelihood values
##' @author Sebastian Funk
nbinom_size_ll <- function(x, size, prob, mu)
{
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  lgamma(size * x + (x - 1)) - (lgamma(size * x) + lgamma(x + 1)) +
    (x - 1) * log (mu / size) -
    (size * x + (x - 1)) * log(1 + mu / size)
}

##' Likelihood of the size of chains with gamma-Borel offspring distribution
##'
##' @param x vector of sizes
##' @param size the dispersion parameter (often called \code{k} in ecological applications)
##' @param prob probability of success (in the parameterisation with \code{prob}, see also \code{\link[stats]{NegBinomial}})
##' @param mu mean parameter
##' @return log-likelihood values
##' @author Sebastian Funk
gborel_size_ll <- function(x, size, prob, mu) {
  if (!missing(prob)) {
    if (!missing(mu)) stop("'prob' and 'mu' both specified")
    mu <- size * (1 - prob) / prob
  }
  lgamma(size + x - 1) - (lgamma(x + 1) + lgamma(size)) - size * log(mu / size) +
    (x - 1) * log(x) - (size + x - 1) * log(x + size / mu)
}

##' Likelihood of the length of chains with Poisson offspring distribution
##'
##' @param x vector of sizes
##' @param lambda rate of the Poisson distributino
##' @return log-likelihood values
##' @author Sebastian Funk
pois_length_ll <- function(x, lambda) {

  ## iterated exponential function
  arg <- exp(lambda * exp(-lambda))
  itex <- 1
  for (i in seq_len(max(x))) itex <- c(itex, arg ^ itex[i])

  Gk <- c(0, exp(-lambda) * itex) ## set G_{0}=1

  log(Gk[x + 1] - Gk[x])
}

##' Likelihood of the length of chains with geometric offspring distribution
##'
##' @param x vector of sizes
##' @param prob probability of the geometric distribution with mean \code{1/prob}
##' @return log-likelihood values
##' @author Sebastian Funk
geom_length_ll <- function(x, prob) {

  lambda <- 1 / prob
  ## G(k) - G(k - 1)
  GkmGkm1 <- (1 - lambda ^ (x)) / (1 - lambda ^ (x + 1)) -
    (1 - lambda ^ (x - 1)) / (1 - lambda ^ (x))

  log(GkmGkm1)
}

##' Likelihood of the length of chains with generic offspring distribution
##'
##' The likelihoods are calculated with a crude approximation using simulated
##'   chains by linearly approximating any missing values in the empirical
##'   cumulative distribution function (ecdf).
##' @param x vector of sizes
##' @param ... any paramaters to pass to \code{\link{chain_sim}}
##' @return log-likelihood values
##' @author Sebastian Funk
##' @inheritParams chain_ll
##' @inheritParams chain_sim
offspring_ll <- function(x, offspring, stat, n=100, ...) {

  dist <- chain_sim(n, offspring, stat, ...)

  ## linear approximation
  f <- ecdf(dist)
  acdf <- diff(c(0, approx(unique(dist), f(unique(dist)), seq_len(max(dist[is.finite(dist)])))$y))
  lik <- acdf[x]
  lik[is.na(lik)] <- 0
  log(lik)
}

##' Likelihood for the outcome of a branching process
##'
##' @param x vector of sizes or lengths of transmission chains
##' @param ... parameters for the offspring distribution
##' @param stat statistic given as \code{x} ("size" or "length" of chains)
##' @param infinite any chains of this size/length will be treated as infinite
##' @param exclude any sizes/lengths to exclude from the likelihood calculation
##' @return likelihood
##' @inheritParams chain_sim
##' @seealso pois_size_ll nbinom_size_ll gborel_size_ll pois_length_ll geom_length_ll offspring_ll
##' @author Sebastian Funk
chain_ll <- function(x, offspring, ..., stat=c("size", "length"), infinite = Inf, exclude)
{
  stat <- match.arg(stat)

  if (any(x >= infinite)) {
    calc_sizes <- seq_len(infinite-1)
    x[x >= infinite] <- infinite
  } else {
    calc_sizes <- unique(x)
  }

  ## first, get likelihood function as given by `offspring` and `stat``
  likelihoods <- c()
  ll_func <- paste(offspring, stat, "ll", sep="_")
  pars <- as.list(unlist(list(...))) ## converts vectors to lists

  ## calculate likelihoods
  if (exists(ll_func)) {
    func <- get(ll_func)
    if (!is.function(func)) stop("'", ll_func, "' is not a function.")
    likelihoods[calc_sizes] <- do.call(func, c(list(x=calc_sizes), pars))
  } else {
    likelihoods[calc_sizes] <-
      do.call(offspring_ll,
              c(list(x=calc_sizes, offspring=offspring, stat=stat), pars))
  }

  if (!missing(exclude)) {
    likelihoods <- likelihoods - log(-expm1(sum(likelihoods[exclude])))
    likelihoods[exclude] <- -Inf
  }

  if (any(x >= infinite)) {
    maxl <- log1p(-sum(exp(likelihoods), na.rm = TRUE))
    likelihoods <- c(likelihoods, maxl)
  }
  chain_likelihoods <- likelihoods[x]

  return(sum(chain_likelihoods))
}
