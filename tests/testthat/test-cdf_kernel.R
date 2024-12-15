test_that("cdf.kernel works", {
  expect_equal(cdf.kernel(0, 0, 1, "gaussian")$cdf, round(pnorm(0, 0, 1), 5))
})


# devtools::load_all()
# testthat::test_file("tests/testthat/test-cdf.kernel.R")
# devtools::test()
# devtools::check()
