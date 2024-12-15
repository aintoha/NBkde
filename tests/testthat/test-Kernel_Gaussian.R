test_that("Kernel Gaussian works", {
  expect_equal(gaussian.kernel(Inf), 0)
  expect_equal(gaussian.kernel(-Inf), 0)
})

test_that("gaussian pdf works", {
  expect_equal(gaussian.pdf(Inf,c(1,0, 1) , 1), 0)
  expect_equal(gaussian.pdf(-Inf, c(1,0, 1) , 1), 0)
})

test_that("gaussian cdf works", {
  expect_equal(gaussian.cdf(Inf), 1)
  expect_equal(gaussian.cdf(-Inf), 0)
})


test_that("gaussian.p2norm works", {
  expect_equal(gaussian.p2norm(0), 1/(2 * sqrt(pi)))
})

test_that("gaussian.ccdf works", {
  expect_equal(gaussian.ccdf(0, 0), 1 / 2)
})

# devtools::load_all()
# testthat::test_file("tests/testthat/test-Kernel_Gaussian.R")
# devtools::test()
# devtools::check()
