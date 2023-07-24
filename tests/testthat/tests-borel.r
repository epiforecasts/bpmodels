test_that("We can calculate probabilities and sample", {
  expect_gt(dborel(1, 0.5), 0)
  expect_identical(dborel(1, 0.5, log = TRUE), -0.5)
  expect_length(rborel(2, 0.9), 2)
})

test_that("Errors are thrown", {
  expect_error(
    dborel(x = 0, mu = 0.5),
    "'x' must be greater than 0"
  )
  expect_error(
    dborel(x = 1, mu = -0.5),
    "'mu' must be greater 0 but less than Inf"
  )
  expect_error(
    dborel(x = 1, mu = Inf),
    "'mu' must be greater 0 but less than Inf"
  )
  expect_error(
    rborel(n = 0, mu = -0.5),
    "'mu' must be greater 0 but less than Inf"
  )
  expect_error(
    rborel(n = 0, mu = Inf),
    "'mu' must be greater 0 but less than Inf"
  )
})
