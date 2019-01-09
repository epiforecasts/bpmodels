##' Simulate chains using a branching process
##'
##' @param n number of simulations to run.
##' @param offspring offspring distribution as character string, e.g. "pois" for
##'     the Poisson offspring distribution. 
##' @param stat statistic to calculate ("size" or "length" of chains)
##' @param infinite a size or length from which the size/length is to be considered infinite
##' @param ... parameters of the offspring distribution
##' @return a vector of sizes/lengths
##' @author Sebastian Funk
chain_sim <- function(n, offspring, stat = c("size", "length"), infinite = Inf, ...) {

    stat <- match.arg(stat)

    ## first, get random function as given by `offspring`
    random_func <- paste0("r", offspring)
    if (!exists(random_func)) stop("Random sampling function '", random_func, "' does not exist.")
    func <- get(random_func)
    if (!is.function(func)) stop("'", random_func, "' is not a function.")

    ## next, simulate n chains
    dist <- c()
    for (i in seq_len(n)) {
        stat_track <- 1 ## variable to track length or size (depending on `stat`)
        state <- 1
        while (state > 0 && state < infinite) {
            offspring <- sum(func(n=state, ...))
            if (stat=="size") {
                stat_track <- stat_track + offspring
            } else if (stat=="length"){
                if (offspring > 0) stat_track <- stat_track + 1
            } else {
                stop("Unknown statistic: '", stat, "'.")
            }
            state <- offspring
        }
        if (state >= infinite) stat_track <- Inf
        dist[i] <- stat_track
    }

    return(dist)
}

