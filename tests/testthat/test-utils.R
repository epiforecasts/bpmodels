test_that("Util functions work", {
  expect_length(rnbinom_mean_disp(n = 5, mn = 4, disp = 2), 5)
  expect_length(rgen_length(n = 1, x = c(1, 2, 3), prob = 0.3), 3)
  expect_length(rbinom_size(n = 1, x = c(1, 2, 3), prob = 0.3), 3)
  expect_identical(complementary_logprob(x = 0), -Inf)
  expect_identical(complementary_logprob(x = -Inf), 0)
  expect_lt(complementary_logprob(x = -0.1), 0)
})

test_that("Errors are thrown", {
  expect_error(rnbinom_mean_disp(n = 5, mn = 4, disp = 0.9),
               "'disp' must be at least 1"
               )
})

test_that("Warnings are thrown", {
  expect_warning(complementary_logprob(0.1), "NaNs produced")
  expect_warning(complementary_logprob(Inf), "NaNs produced")
})
