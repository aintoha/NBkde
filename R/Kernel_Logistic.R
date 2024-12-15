


Logistic.kernel <- function(x){

  return(1 / (2 + exp(x) + exp(-x)))

}


Logistic.cdf <- function(x){

  return(exp(x) / (1 + exp(x)))

}

#' Calculate the p2Norm of a Logistic kernel from negative infinity to infinity
#' @param x The value of x
#' @return a numerical / vector

Logistic.p2Norm <- function(x){

  val = ((x - 2) * exp(2 *x) + (x + 2) * exp(x)) / (exp(3 * x) - 3 * exp(2 * x) + 3 * exp(x) -1)

  return(val)

}

#' Calculate the p2Norm of a Logistic kernel cdf from negative infinity to a value a
#' @param x The value of x
#' @param a A numeric for the upper found of the integral
#' @return a numerical / vector

Logistic.cdf.p2Norm <- function(x, a){

  val = ((x - 2) * exp(2 *x) + (x + 2) * exp(x)) / (exp(3 * x) - 3 * exp(2 * x) + 3 * exp(x) -1)

  return(val)

}

