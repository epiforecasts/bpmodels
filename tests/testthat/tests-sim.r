context("Simulating from a branching process model")

test_that("Chains can be simulated",
{
    expect_length(chain_sim(n=2, "pois", lambda=0.5), 2)
    expect_length(chain_sim(n=10, "pois", "length", lambda=0.9), 10)
    expect_true(is.data.frame(chain_sim(n=10, "pois", lambda=2, tree=TRUE,
                                        infinite=10)))
    expect_false(any(is.finite(chain_sim(n=2, "pois", "length", lambda=0.5,
                                         infinite=1))))
    expect_no_error(chain_sim(n = 2, offspring = 'pois', "size", lambda = 0.9, 
                                               tree = TRUE)
                    )
})

test_that("Errors are thrown",
{
    expect_error(chain_sim(n=2, "dummy"), "does not exist")
    expect_error(chain_sim(n=2, "lnorm", meanlog=log(1.6)), "integer")
    expect_error(chain_sim(n = 2, offspring = pois, "length", lambda = 0.9), 
                 "not found"
                 )
    expect_error(chain_sim(n = 2, offspring = 'pois', "size", lambda = 0.9, 
                           serial = c(1:2), "must be a function")
                 )
    expect_error(chain_sim(n = 2, offspring = c(1, 2), "length", lambda = 0.9),
                 "not a character string")
    expect_error(chain_sim(n = 2, offspring = list(1, 2), "length", lambda = 0.9),
                 "not a character string")
    expect_error(chain_sim(n = 2, offspring = 'pois', "size", lambda = 0.9, 
                           serial = function(x) rpois(x, 0.9), tree = FALSE),
                 "If `serial` is specified, then `tree` cannot be set to `FALSE`."
                 )
    expect_error(chain_sim(n = 2, offspring = 'pois', "size", lambda = 0.9, 
                           tf = 5, tree = FALSE),
                 "If `tf` is specified, `serial` must be specified too."
    )
})

context("Simulating from a branching process model
    accounting for depletion of susceptibles")


test_that("Chains can be simulated",
{
    expect_true(
        is.data.frame(
            chain_sim_susc(
                "pois",
                mn_offspring = 2,
                serial = function(x) 3,
                pop = 100
            )
        )
    )

    expect_true(
        is.data.frame(
            chain_sim_susc(
                "nbinom",
                mn_offspring = 2,
                disp_offspring = 1.5,
                serial = function(x) 3,
                pop = 100
            )
        )
    )

    expect_true(
        nrow(
            chain_sim_susc(
                "pois",
                mn_offspring = 2,
                serial = function(x) 3,
                pop = 1
            )
        ) == 1
    )

    expect_true(
        nrow(
            chain_sim_susc(
                "pois",
                mn_offspring = 100,
                tf = 2,
                serial = function(x) 3,
                pop = 999
            )
        ) == 1
    )

    expect_true(
        nrow(
            chain_sim_susc(
                "pois",
                mn_offspring = 100,
                serial = function(x) 3,
                pop = 999,
                initial_immune = 998
            )
        ) == 1
    )

})

test_that("Errors are thrown",
{
    expect_error(
        chain_sim_susc(
            "dummy",
            mn_offspring = 3,
            serial = function(x) 3,
            pop = 100),
        paste0("'arg' should be one of ", dQuote('pois'), ', ', dQuote('nbinom')))
    expect_error(
        chain_sim_susc(
            "nbinom",
            mn_offspring = 3,
            disp_offspring = 1,
            serial = function(x) 3,
            pop = 100
            ),
        "Offspring distribution 'nbinom' requires argument
                disp_offspring > 1. Use 'pois' if there is no overdispersion.")
    expect_error(
        chain_sim_susc(
            "nbinom",
            mn_offspring = 3,
            serial = function(x) 3,
            pop = 100
            ),
        "argument \"disp_offspring\" is missing, with no default")

})

test_that('warnings work as expected', {
  expect_warning(
    chain_sim_susc(
      "pois",
      mn_offspring = 3,
      disp_offspring = 1,
      serial = function(x) 3,
      pop = 100
      ),
    "argument disp_offspring not used for
                poisson offspring distribution."
    )
})
