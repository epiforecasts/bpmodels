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

test_that("Analytical size or length distributions are correctly calculated", {
  expect_equal(
    round(chain_ll(chains, "pois", "size", lambda = 0.5), 6),
    -8.607196
  )
  expect_equal(
    round(chain_ll(chains, "nbinom", "size", mu = 0.5, size = 0.2), 6),
    -9.134369
  )
  expect_equal(
    round(chain_ll(chains, "nbinom", "size", prob = 0.5, size = 0.2), 6),
    -10.88944
  )
  expect_equal(
    round(chain_ll(chains, "gborel", "size", prob = 0.5, size = 0.2), 6),
    -11.21929
  )
  expect_equal(
    round(chain_ll(chains, "pois", "length", lambda = 0.5), 6),
    -9.399452
  )
  expect_equal(
    round(chain_ll(chains, "geom", "length", prob = 0.5), 6),
    -12.48639
  )
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
