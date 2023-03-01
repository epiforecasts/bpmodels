context("Calculating the likelihood from a branching process model")

chains <- c(1, 1, 4, 7)

test_that("Likelihoods can be calculated", {
  expect_lt(chain_ll(chains, "pois", "size", lambda = 0.5), 0)
  expect_lt(chain_ll(chains, "pois", "size", lambda = 0.5, exclude = 1), 0)
  expect_lt(chain_ll(chains, "pois", "size", lambda = 0.5, infinite = 5), 0)
  expect_lt(chain_ll(chains, "pois", "size",
    lambda = 0.5, obs_prob = 0.5,
    nsim_obs = 1
  ), 0)
  expect_lt(chain_ll(chains, "pois", "length",
    lambda = 0.5, obs_prob = 0.5,
    nsim_obs = 1
  ), 0)
  expect_lt(chain_ll(chains, "pois", "size",
    lambda = 0.5, infinite = 5,
    obs_prob = 0.5, nsim_obs = 1
  ), 0)
  expect_lt(chain_ll(chains, "binom", "size", size = 1, prob = 0.5), 0)
})

test_that("Analytical size or length distributions are implemented", {
  expect_true(all(pois_size_ll(chains, lambda = 0.5) < 0))
  expect_true(all(nbinom_size_ll(chains, mu = 0.5, size = 0.2) < 0))
  expect_true(all(nbinom_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(gborel_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(gborel_size_ll(chains, prob = 0.5, size = 0.2) < 0))
  expect_true(all(pois_length_ll(chains, lambda = 0.5) < 0))
  expect_true(all(geom_length_ll(chains, prob = 0.5) < 0))
})

test_that("Errors are thrown", {
  expect_error(
    chain_ll(chains, list(), "size", lambda = 0.5),
    "not a character"
  )
  expect_error(
    chain_ll(chains, "pois", "size", lambda = 0.5, obs_prob = 3),
    "must be within"
  )
  expect_error(
    chain_ll(chains, "pois", "size", lambda = 0.5, obs_prob = 0.5),
    "must be specified"
  )
  expect_error(
    nbinom_size_ll(chains, mu = 0.5, size = 0.2, prob = 0.1),
    "both specified"
  )
  expect_error(
    gborel_size_ll(chains, mu = 0.5, size = 0.2, prob = 0.1),
    "both specified"
  )
})
