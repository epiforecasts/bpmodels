##' Simulate chains using a branching process
##'
##' @param n number of simulations to run.
##' @param offspring offspring distribution, given as the function used to
##'     generate the number of offspring in each generation, e.g. `rpois` for
##'     Poisson distributed offspring
##' @param stat statistic to calculate ("size" or "length" of chains)
##' @param infinite a size or length from which the size/length is to be
##'     considered infinite
##' @param ... parameters of the offspring distribution
##' @return a vector of sizes/lengths
##' @author Sebastian Funk
##' @export
##' @examples
##' chain_sim(n=5, rpois, "size", lambda=0.5)
chain_sim <- function(n, offspring, stat = c("size", "length"), infinite = Inf,
                      ...) {

    stat <- match.arg(stat)

    ## first, get random function as given by `offspring`
    if (!is.function(offspring)) {
        stop("object passed as 'offspring' is not a function.")
    }

    ## next, simulate n chains
    dist <- c()
    for (i in seq_len(n)) {
        stat_track <- 1 ## track length or size (depending on `stat`)
        state <- 1
        while (state > 0 && state < infinite) {
            n_offspring <- sum(offspring(n=state, ...))
            if (n_offspring %% 1 > 0) {
                stop("Offspring distribution must return integers")
            }
            if (stat=="size") {
                stat_track <- stat_track + n_offspring
            } else if (stat=="length") {
                if (n_offspring > 0) stat_track <- stat_track + 1
            }
            state <- n_offspring
        }
        if (state >= infinite) stat_track <- Inf
        dist[i] <- stat_track
    }

    return(dist)
}

