test_that("Chains can be simulated", {
  expect_length(chain_sim(n = 2, "pois", lambda = 0.5), 2)
  expect_length(chain_sim(n = 10, "pois", "length", lambda = 0.9), 10)
  expect_s3_class(
    chain_sim(n = 10,
              "pois",
              lambda = 2,
              tree = TRUE,
              infinite = 10
              ),
    "data.frame"
    )
  expect_false(any(is.finite(chain_sim(
    n = 2, "pois", "length", lambda = 0.5,
    infinite = 1
  ))))
  expect_no_error(chain_sim(
    n = 2, offspring = "pois", "size", lambda = 0.9,
    tree = TRUE
  ))
})

test_that("Errors are thrown", {
  expect_error(chain_sim(n = 2, "dummy"), "does not exist")
  expect_error(chain_sim(n = 2, "lnorm", meanlog = log(1.6)), "integer")
  expect_error(
    chain_sim(n = 2, offspring = pois, "length", lambda = 0.9),
    "not found"
  )
  expect_error(chain_sim(
    n = 2, offspring = "pois", "size", lambda = 0.9,
    serial = c(1, 2), "must be a function"
  ))
  expect_error(
    chain_sim(n = 2, offspring = c(1, 2), "length", lambda = 0.9),
    "not a character string"
  )
  expect_error(
    chain_sim(n = 2, offspring = list(1, 2), "length", lambda = 0.9),
    "not a character string"
  )
  expect_error(
    chain_sim(
      n = 2,
      offspring = "pois",
      "size",
      lambda = 0.9,
      tf = 5,
      tree = FALSE
    ),
    "If `tf` is specified, `serial` must be specified too."
  )
})

test_that("Chains can be simulated", {
  expect_s3_class(
      chain_sim_susc(
        "pois",
        mn_offspring = 2,
        serial = function(x) 3,
        pop = 100
      ),
      "data.frame"
  )

  expect_s3_class(
      chain_sim_susc(
        "nbinom",
        mn_offspring = 2,
        disp_offspring = 1.5,
        serial = function(x) 3,
        pop = 100
      ),
      "data.frame"
  )

  expect_identical(
    nrow(
      chain_sim_susc(
        "pois",
        mn_offspring = 2,
        serial = function(x) 3,
        pop = 1
      )
    ),
    1L
  )

  expect_identical(
    nrow(
      chain_sim_susc(
        "pois",
        mn_offspring = 100,
        tf = 2,
        serial = function(x) 3,
        pop = 999
      )
    ),
    1L
  )

  expect_identical(
    nrow(
      chain_sim_susc(
        "pois",
        mn_offspring = 100,
        serial = function(x) 3,
        pop = 999,
        initial_immune = 998
      )
    ),
    1L
  )
})

test_that("Errors are thrown", {
  expect_error(
    chain_sim_susc(
      "dummy",
      mn_offspring = 3,
      serial = function(x) 3,
      pop = 100
    ),
    paste0("'arg' should be one of ", dQuote("pois"), ", ", dQuote("nbinom"))
  )
  expect_error(
    chain_sim_susc(
      "nbinom",
      mn_offspring = 3,
      disp_offspring = 1,
      serial = function(x) 3,
      pop = 100
    ),
    paste("Offspring distribution 'nbinom'",
          "requires argument 'disp_offspring' > 1.",
          "Use 'pois' if there is no overdispersion."
          )
    )
  expect_error(
    chain_sim_susc(
      "nbinom",
      mn_offspring = 3,
      serial = function(x) 3,
      pop = 100
    ),
    "Argument 'disp_offspring' was not specified."
  )
})

test_that("warnings work as expected", {
  expect_warning(
    chain_sim_susc(
      "pois",
      mn_offspring = 3,
      disp_offspring = 1,
      serial = function(x) 3,
      pop = 100
    ),
    "Argument 'disp_offspring' not used for poisson offspring distribution."
  )
  expect_warning(
    chain_sim(
      n = 2,
      offspring = "pois",
      "size",
      lambda = 0.9,
      serial = function(x) rpois(x, 0.9),
      tree = FALSE
    ),
    sprintf("%s %s",
            "`serial` can't be used with `tree = FALSE`;",
            "Setting `tree = TRUE` internally."
    )
  )
})
