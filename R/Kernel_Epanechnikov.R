#' Calculating probability density functions for Epanechnikov kernel
#'
#' @param x vector of observations.
#' @return A vector of score values.
#' examples:
#' epanechnikov.kernel(0.45);
#' @export

epanechnikov.kernel <- function(x) {


  val =  ifelse(x > -1 & x < 1, 3/4 * (1 - x^2), 0)
  return(val)

}

#' Calculating cumulative probability functions for Epanechnikov kernel
#'
#' @param x vector of observations.
#' @return A vector of score values.
#' examples:
#' epanechnikov.cdf(0.45);
#' @export

epanechnikov.cdf <- function(x) {

  n = length(x)

  val = rep(NA)

  for (i in 1:n) {

     if (x[i] <= -1){

      val[i] = 0

    } else if (x[i] >= 1) {

      val[i] = 1

    } else {

      val[i] = (-x[i]^3 + 3 * x[i] + 2) / 4

      }

    }
  return(val)
}


#' Calculating L2-norm of for Epanechnikov kernel
#'
#' @param x vector of observations.
#' @return A vector of score values.
#' examples:
#' epanechnikov.p2norm(0.45);
#' @export

epanechnikov.kernel <- function(x) {


  val =  ifelse(x > -1 & x < 1, 3/4 * (1 - x^2), 0)
  return(val)

}

#' L2-norm Epanechnikov kernel
#'
#' Calculating L2-norm of Epanechnikov kernel from negative infinity to infinity
#'
#' @param x vector of observations.
#' @return A vector of score values.
#' examples:
#' epanechnikov.p2norm(0.45);
#' @export

epanechnikov.p2norm <- function(x) {

  n = length(x)

  val = rep(NA)

  for (i in 1:n) {

    if (x[i] <= -1){

      val[i] = 0

    } else if (x[i] >= 1) {

      val[i] = 1

    } else {

      val[i] = (-x[i]^3 + 3 * x[i] + 2) / 4

    }

  }
  return(val)
}
