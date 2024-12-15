test_that("pdf_kernel works", {
  expect_equal(pdf_kernel(0, 0, 1, "gaussian")$pdf, dnorm(0, 0, 1))
})


# devtools::load_all()
# testthat::test_file("tests/testthat/test-pdf_kernel.R")
# devtools::test()
# devtools::check()
