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

    stat_track <- rep(1, n) ## track length or size (depending on `stat`)
    n_offspring <- rep(1, n) ## current number of offspring
    sim <- seq_len(n) ## track chains that are still being simulated

    ## next, simulate n chains
    while (length(sim) > 0) {
        ## simulate next generation
        next_gen <- offspring(n=sum(n_offspring[sim]), ...)
        if (any(next_gen %% 1 > 0)) {
            stop("Offspring distribution must return integers")
        }
        ## record indices corresponding the number of offspring of last
        ## iteration, for the tapply call below
        indices <- rep(sim, n_offspring[sim])
        ## initialise number of offspring
        n_offspring <- rep(0, n)
        ## assign offspring sum to indices still being simulated
        n_offspring[sim] <- tapply(next_gen, indices, sum)
        ## track size/length
        if (stat=="size") {
            stat_track <- stat_track + n_offspring
        } else if (stat=="length") {
            stat_track <- stat_track + pmin(1, n_offspring)
        }
        ## only continue to simulate chains that offspring and aren't of
        ## infinite size/length
        sim <- which(n_offspring > 0 & stat_track < infinite)
    }

    stat_track[stat_track >= infinite] <- Inf

    return(stat_track)
}

