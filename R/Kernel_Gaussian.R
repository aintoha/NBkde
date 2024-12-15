#' Pdf of Gaussian kernel functions
#'
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' kernel.gaussian(10);
#' @export

kernel.gaussian <- function(x) {

  value =  1/ (sqrt(2 *pi)) * exp(-0.5 * x^2)
  param = "gaussian"
  structure(list(pdf = value, parameter = param),
            class = c("kernel", "gaussian", "Distribution"))

}

#' Calculating the pdf of the Gaussian kernel functions
#'
#' @param x A numerical value
#' @param data A vector of values
#' @param h bandwidth
#' @returns A list
#' @examples
#' x <- 1
#' data <- rnorm(10, 2, 1)
#' h <- 0.5
#' pdf.gaussian(x, data, h)
#' @export

pdf.gaussian <- function(x, data, h) {

  dif = sapply(data, function (y) (x - y) / h)

  pdf = gaussian.kernel(dif) / h

  param = list(param = "gaussian")
  structure(list("pdf" = mean(pdf), parameter = param),
            class = c("pdf", "gaussian", "Distribtuion"))
}

#' Cdf for Gaussian kernel functions
#'
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' cdf.gaussian(10);
#' @export

cdf.gaussian <- function(x) {

  value = 0.5 * pracma::erf(x / sqrt(2)) + 0.5
  param = list(param = "gaussian")
  structure(list("cdf" = value, parameter = param),
            class = c("cdf", "gaussian", "Distribtuion"))

}

#' L2-norm of a Gaussian kernel
#'
#' @description This function calculates the p2-norm of Gaussian kernel
#'
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' p2norm.gaussian(10);
#' @export

p2norm.gaussian <- function(data){

  value = 1 / (2 * sqrt(pi)) * exp((- (data)^2) / 4)
  param = list(kernel = "gaussian")
  structure(list("p2norm" = value, parameter = param),
            class = c("p2norm", "gaussian", "Distribtuion"))

}

#' Cumulative cdf of Gaussian Kernel Function
#'
#' @description This function calculates the integral of a Gaussian kernel cdf from
#' negative infinity up until a value
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' ccdf.gaussian(10);
#' @export

ccdf.gaussian <- function(x) {

  a = value1
  b = value2
  value = 0.5 * (1 +  (pracma::erf(a - b) / sqrt(2)))
  param = list(param = "gaussian")
  structure(list("ccdf" = value, parameter = param),
            class = c("cdf", "gaussian", "Distribtuion"))

}

#' Logloss for  Gaussian Kernel Function
#'
#' @description This function calculates the log-loss of a Gaussian kernel cdf from
#' negative infinity up until a value
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' logloss.gaussian(10);
#' @export

logloss.gaussian <- function(x) {

  value = -log(kernel.gaussian(x))
  param = list(param = "gaussian")
  structure(list("loss" = value, parameter = param),
            class = c("logloss", "gaussian", "Distribtuion"))

}
