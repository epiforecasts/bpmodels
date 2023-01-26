#' Simulate transmission chains using a branching process
#' @description \code{chain_sim()} is a stochastic simulator for generating 
#' transmission chain data given information on the offspring distribution, 
#' serial interval, time since the first case, etc. 
#' @param n Number of simulations to run.
#' @param offspring Offspring distribution: a character string corresponding to
#'   the R distribution function (e.g., "pois" for Poisson, where
#'   \code{\link{rpois}} is the R function to generate Poisson random numbers) 
#' @param stat String; Statistic to calculate. Can be one of:
#' \itemize{
#'   \item "size": the total number of offspring.
#'   \item "length": the total number of ancestors. 
#' }
#' @param infinite A size or length above which the simulation results should be 
#' set to `Inf`. Defaults to `Inf`, resulting in no results ever set to `Inf`
#' @param tree Logical. Should the transmission tree be returned? Defaults to `FALSE`.
#' @param serial The serial interval generator function; the name of a user-defined 
#' named or anonymous function with only one argument `n`, representing the number 
#' of serial intervals to generate.
#' @param t0 Start time (if serial interval is given); either a single value or a 
#' vector of length `n` (number of simulations) with initial times. Defaults to 0.  
#' @param tf End time (if serial interval is given).
#' @param ... Parameters of the offspring distribution as required by R.
#' @return Either: 
#' \itemize{
#'  \item{A vector of sizes/lengths (if \code{tree == FALSE} OR serial
#'   interval function not specified, since that implies \code{tree == FALSE})}, or 
#'   \item {a data frame with 
#'   columns `n` (simulation ID), `time` (if the serial interval is given) and 
#'   (if \code{tree == TRUE}), `id` (a unique ID within each simulation for each 
#'   individual element of the chain), `ancestor` (the ID of the ancestor of each 
#'   element), and `generation`.}
#' }
#' @author Sebastian Funk, James M. Azam
#' @export
#' @details 
#' `chain_sim()` either returns a vector or a data.frame. The output is either a 
#' vector if `serial` is not provided, which automatically sets \code{tree = FALSE},
#' or a `data.frame`, which means that `serial` was provided as a function. When `serial`
#' is provided, it means \code{tree = TRUE} automatically. However, setting 
#' \code{tree = TRUE} would require providing a function for `serial`.
#' 
#' # The serial interval (`serial`):
#' 
#' ## Assumptions/disambiguation
#' 
#' In epidemiology, the generation interval is the duration between successive 
#' infectious events in a chain of transmission. Similarly, the serial interval is the 
#' duration between observed symptom onset times between successive 
#' cases in a transmission chain. The generation interval is often hard to observe 
#' because exact times of infection are hard to measure hence, the serial interval
#' is often used instead. Here, we use the serial interval to represent what would 
#' normally be called the generation interval, that is, the time between successive
#' cases. 
#' 
#' ## Specifying `serial` in `chain_sim()`
#' 
#' `serial` must be specified as a named or 
#' [anonymous/inline/unnamed function](https://en.wikipedia.org/wiki/Anonymous_function#R) 
#' with one argument. 
#' 
#' If `serial` is specified, `chain_sim()` returns times of 
#' infection as a column in the output. Moreover, specifying a function for `serial` implies 
#' \code{tree = TRUE} and a tree of infectors (`ancestor`) and infectees (`id`) 
#' will be generated in the output. 
#' 
#' For example, assuming we want to specify the serial interval 
#' generator as a random log-normally distributed variable with `meanlog = 0.58` 
#' and `sdlog = 1.58`, we could define a named function, let's call it 
#' "serial_interval", with only one argument representing the number of serial 
#' intervals to sample: \code{serial_interval <- function(n){rlnorm(n, 0.58, 1.38)}}, 
#' and assign the name of the function to serial in `chain_sim()` like so 
#' \code{chain_sim(..., serial = serial_interval)}, 
#' where `...` are the other arguments to `chain_sim()`. Alternatively, we 
#' could assign an anonymous function to serial in the `chain_sim()` call like so
#' \code{chain_sim(..., serial = function(n){rlnorm(n, 0.58, 1.38)})}, 
#' where `...` are the other arguments to `chain_sim()`.
#' @examples
#' # Specifying no `serial` and `tree == FALSE` (default) returns a vector
#' set.seed(123)
#' chain_sim(n = 5, offspring = "pois", stat = "size", lambda = 0.5, tree = FALSE)
#' 
#' # Specifying `serial` without specifying `tree` will set `tree = TRUE` internally.
#'  
#' # We'll first define the serial function 
#' set.seed(123)
#' serial_interval <- function(n){rlnorm(n, meanlog = 0.58, sdlog = 1.58)}
#' chain_sim(n = 5, offspring = 'pois', lambda = 0.5, stat = 'length', infinite = 100, 
#' serial = serial_interval)
#' 
#' # Specifying `serial` and `tree = FALSE` will throw an error 
#' set.seed(123)
#' \dontrun{
#' try(chain_sim(n = 10, serial = function(x) 3, offspring = "pois", lambda = 2, 
#' infinite = 10, tree = FALSE)
#' )
#' }
chain_sim <- function(n, offspring, stat = c("size", "length"), infinite = Inf,
                      tree = FALSE, serial, t0 = 0, tf = Inf, ...) {

    stat <- match.arg(stat)

    ## first, get random function as given by `offspring`
    if (!is.character(offspring)) {
        stop("object passed as 'offspring' is not a character string. Did you forget
             to enclose it in quotes?")
    }

    roffspring_name <- paste0("r", offspring)
    if (!(exists(roffspring_name)) || !is.function(get(roffspring_name))) {
        stop("Function ", roffspring_name, " does not exist.")
    }

    if (!missing(serial)) {
        if (!is.function(serial)) {
            stop("The `serial` argument must be a function (see details in ?chain_sim()).")
        }
        if (!missing(tree) && tree == FALSE) {
            warning("`serial` can't be used with `tree = FALSE`; Setting `tree = TRUE` internally.")
          tree <- TRUE
          }
        tree <- TRUE
    } else if (!missing(tf)) {
        stop("The `tf` argument needs a `serial` argument.")
    }

    stat_track <- rep(1, n) ## track length or size (depending on `stat`)
    n_offspring <- rep(1, n) ## current number of offspring
    sim <- seq_len(n) ## track chains that are still being simulated

    ## initialise data frame to hold the trees
    if (tree) {
        generation <- 1L
        tdf <-
            data.frame(n = seq_len(n),
                       id = 1L,
                       ancestor = NA_integer_,
                       generation = generation)

        ancestor_ids <- rep(1, n)
        if (!missing(serial)) {
            tdf$time <- t0
            times <- tdf$time
        }
    }

    ## next, simulate n chains
    while (length(sim) > 0) {
        ## simulate next generation
        next_gen <- get(roffspring_name)(n=sum(n_offspring[sim]), ...)
        if (any(next_gen %% 1 > 0)) {
            stop("Offspring distribution must return integers")
        }

        ## record indices corresponding to the number of offspring
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

        ## record times/ancestors (if tree==TRUE)
        if (tree && sum(n_offspring[sim]) > 0) {
            ancestors <- rep(ancestor_ids, next_gen)
            current_max_id <- unname(tapply(ancestor_ids, indices, max))
            indices <- rep(sim, n_offspring[sim])
            ids <- rep(current_max_id, n_offspring[sim]) +
                unlist(lapply(n_offspring[sim], seq_len))
            generation <- generation + 1L
            new_df <-
                data.frame(n = indices,
                           id = ids,
                           ancestor = ancestors,
                           generation = generation)
            if (!missing(serial)) {
                times <- rep(times, next_gen) + serial(sum(n_offspring))
                current_min_time <- unname(tapply(times, indices, min))
                new_df$time <- times
            }
            tdf <- rbind(tdf, new_df)
        }

        ## only continue to simulate chains that offspring and aren't of
        ## infinite size/length
        sim <- which(n_offspring > 0 & stat_track < infinite)
        if (length(sim) > 0) {
            if (!missing(serial)) {
                ## only continue to simulate chains that don't go beyond tf
                sim <- intersect(sim, unique(indices)[current_min_time < tf])
            }
            if (tree) {
                if (!missing(serial)) {
                    times <- times[indices %in% sim]
                }
                ancestor_ids <- ids[indices %in% sim]
            }
        }
    }

    if (tree) {
        if (!missing(tf)) {
            tdf <- tdf[tdf$time < tf, ]
        }
        rownames(tdf) <- NULL
        return(tdf)
    } else {
        stat_track[stat_track >= infinite] <- Inf
        return(stat_track)
    }
}

