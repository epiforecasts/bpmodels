test_that("Util functions work", {
  expect_length(rnbinom_mean_disp(n = 5, mn = 4, disp = 2), 5)
  expect_length(rgen_length(n = 1, x = c(1, 2, 3), prob = 0.3), 3)
  expect_length(rbinom_size(n = 1, x = c(1, 2, 3), prob = 0.3), 3)
  expect_identical(complementary_logprob(x = 0), -Inf)
  expect_identical(complementary_logprob(x = -Inf), 0)
  expect_lt(complementary_logprob(x = -0.1), 0)
})

test_that("Errors are thrown", {
  # Checks on 'disp' argument
  expect_error(
    rnbinom_mean_disp(n = 5, mn = 4, disp = 0.9),
    "is not >= 1"
  )
  expect_error(
    rnbinom_mean_disp(n = 5, mn = 4, disp = NA),
    "May not be NA"
  )
  expect_error(
    rnbinom_mean_disp(n = 5, mn = 4, disp = Inf),
    "Must be finite"
  )
  # Checks on 'n' argument
  expect_error(
    rnbinom_mean_disp(n = 0, mn = 4, disp = 2),
    "is not >= 1"
  )
  expect_error(
    rnbinom_mean_disp(n = NA, mn = 4, disp = 2),
    "May not be NA"
  )
  expect_error(
    rnbinom_mean_disp(n = Inf, mn = 4, disp = 2),
    "Must be finite"
  )
  # Checks on 'mn' argument
  expect_error(
    rnbinom_mean_disp(n = 2, mn = 0, disp = 2)
  )
  expect_error(
    rnbinom_mean_disp(n = 2, mn = NA, disp = 2),
    "May not be NA"
  )
  expect_error(
    rnbinom_mean_disp(n = 2, mn = Inf, disp = 2),
    "Must be finite"
  )
})

test_that("Errors are thrown", {
  expect_error(
    complementary_logprob(0.1),
    "is not <= 0"
  )
  expect_error(
    complementary_logprob(Inf),
    "is not <= 0"
  )
})
