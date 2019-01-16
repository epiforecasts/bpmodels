context("Simulating from a branching process model")

test_that("Chains can be simulated",
{
    expect_length(chain_sim(n=2, "pois", lambda=0.5), 2)
    expect_length(chain_sim(n=10, "pois", "length", lambda=0.9), 10)
    expect_false(any(is.finite(chain_sim(n=2, "pois", "length", lambda=0.5, infinite=1))))
})

test_that("Errors are thrown",
{
    expect_error(chain_sim(n=2, "dummy"), "does not exist")
})
