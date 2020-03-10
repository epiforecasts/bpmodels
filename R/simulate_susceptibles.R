##' Simulate a single chain using a branching process while accounting
##' for depletion of susceptibles.
##'
##' @param offspring offspring distribution: a character string corresponding to
##'   the R distribution function. Currently only "pois" & "nbinom" are
##'   supported. Internally truncated distributions are used to avoid infecting
##'   more people than susceptibles available.
##' @param mn_offspring the average number of secondary cases for each case
##' @param disp_offspring the dispersion coefficient (var/mean) of the number of
##'      secondary cases. Ignored if offspring == "pois". Must be > 1.
##' @param serial the serial interval. A function that takes one parameter
##'     (`n`), the number of serial intervals to randomly sample.
##'     Value must be >= 0.
##' @param t0 start time
##' @param tf end time
##' @param pop the population
##' @param initial_immune the number of initial immunes in the population
##' @return a data frame with columns `time`, `id` (a unique ID for each
##'     individual element of the chain), `ancestor` (the ID of the ancestor
##'      of each element), and `generation`.
##'
##' @details This function has a couple of key differences with chain_sim:
##'     it can only simulate one chain at a time,
##'     it can only handle implemented offspring distributions
##'         ("pois" and "nbinom"),
##'     it always tracks and returns a data frame containing the entire tree,
##'     the maximal length of chains is limited with pop instead of infinite.
##'
##' @author Flavio Finger
##' @export
##' @examples
##' chain_sim_susc("pois", mn_offspring=0.5, serial = function(x) 3, pop = 100)
chain_sim_susc <- function(
    offspring = c("pois", "nbinom"),
    mn_offspring,
    disp_offspring,
    serial,
    t0 = 0,
    tf = Inf,
    pop,
    initial_immune = 0
) {

    offspring <- match.arg(offspring)

    if (offspring == "pois") {
        if (!missing(disp_offspring)) {
            warning("argument disp_offspring not used for
                poisson offspring distribution.")
        }

        ## using a right truncated poisson distribution
        ## to avoid more cases than susceptibles
        offspring_fun <- function(n, susc) {
            truncdist::rtrunc(
                n,
                spec = "pois",
                lambda = mn_offspring * susc / pop,
                b = susc)
            }

    } else if (offspring  == "nbinom") {

        if (disp_offspring <= 1) { ## dispersion index
            stop("Offspring distribution 'nbinom' requires argument
                disp_offspring > 1. Use 'pois' if there is no overdispersion.")
        }

        offspring_fun <- function(n, susc) {
            ## get distribution params from mean and dispersion
            ## see ?rnbinom for parameter definition
            new_mn <- mn_offspring * susc / pop ##apply susceptibility
            size <- new_mn / (disp_offspring - 1)

            ## using a right truncated nbinom distribution
            ## to avoid more cases than susceptibles
            truncdist::rtrunc(
                n,
                spec = "nbinom",
                b = susc,
                mu = new_mn,
                size = size)
        }
    }

    ## initializations
    tdf <- data.frame(
        id = 1L,
        ancestor = NA_integer_,
        generation = 1L,
        time = t0,
        offspring_generated = FALSE
    )

    susc <- pop - initial_immune - 1L
    t <- t0

    ## continue if any unsimulated has t <= tf
    ## AND there is still susceptibles left
    while (
        any(tdf$time[!tdf$offspring_generated] <= tf) &
        susc > 0
        ) {

        ## select from which case to generate offspring
        t <- min(tdf$time[!tdf$offspring_generated]) #lowest unsimulated t

        ## index of the first in df with t, extract vars
        idx <- which(tdf$time == t & !tdf$offspring_generated)[1]
        id_parent <- tdf$id[idx]
        t_parent <- tdf$time[idx]
        gen_parent <- tdf$generation[idx]

        ## generate it
        current_max_id <- max(tdf$id)
        n_offspring <- offspring_fun(1, susc)

        if (n_offspring %% 1 > 0) {
            stop("Offspring distribution must return integers")
        }

        ## mark as done
        tdf$offspring_generated[idx] <- TRUE

        ## add to df
        if (n_offspring > 0) {
            ## draw times
            new_times <- serial(n_offspring)

            if (any(new_times < 0)) {
                stop("Serial interval must be >= 0.")
            }

            new_df <- data.frame(
                id = current_max_id + seq_len(n_offspring),
                time = new_times + t_parent,
                ancestor = id_parent,
                generation = gen_parent + 1L,
                offspring_generated = FALSE
            )

            ## add new cases to tdf
            tdf <- rbind(tdf, new_df)
        }

        ## adjust susceptibles
        susc <- susc - n_offspring
    }

    ## remove cases with time > tf that could
    ## have been generated in the last generation
    tdf <- tdf[tdf$time <= tf, ]

    ## sort output and remove columns not needed
    tdf <- tdf[order(tdf$time, tdf$id), ]
    tdf$offspring_generated <- NULL

    return(tdf)
}