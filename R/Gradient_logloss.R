#' Gradient logloss
#'
#' @description Compute the derivative of negative logarithmic score
#'             for Gaussian kernel for a vector using a single bandwidth
#'
#' @param x  the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#'
#' @returns value
#' @examples
#' data <- seq(0.1, 0.5, by = 0.1)
#' x <- c(0.1, 0.2, 0.3,  0.15)
#' bandwidth = 0.1
#' gradient_logloss(x, data, bandwidth)
#' @export


gradient_logloss = function(x, data, bandwidth){

  h = bandwidth
  dif = sapply(x, function(x) (x - data) / h)
  exp_dif = exp(-dif^2 / 2)
  prod_exp_dif = exp_dif * (dif^2 / h)
  sum_exp_dif = colSums(exp_dif)
  sum_prod_exp_dif = colSums(prod_exp_dif)

  gradient = sum(1 / h - (1/sum_exp_dif * sum_prod_exp_dif))

  return(gradient)

}
