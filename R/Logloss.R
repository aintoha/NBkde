#' Log-loss
#'
#' @description Compute the negative logarithmic score
#'             for Gaussian kernel for a vector using a single bandwidth
#'
#' @param x  the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#' @param kernel_name A string
#'
#' @returns x a vector of x
#' @returns kernel_name
#' @returns bandwidth
#' @returns logloss A vector of loss evaluate at x
#' @returns Average_loss The mean of the logloss
#'
#' @examples
#' data <- seq(0.1, 0.5, by = 0.1)
#' x <- c(0.1, 0.2, 0.3,  0.15)
#' bandwidth = 0.1
#' kernel_name = "gaussian"
#' log_loss(x=x, data, bandwidth, kernel_name= kernel_name)
#' @export

log_loss <- function(x, data, bandwidth, kernel_name){

  loss = - log(pdf_kernel(x = x, data = data, bandwidth = bandwidth, kernel_name=kernel_name)$pdf)
  mean_loss = mean(loss)
  results = structure(list("x" = x, "kernel" = kernel_name,
                           "bandwidth" = bandwidth,
                           "log_loss" = loss,
                           "average_loss" = mean_loss))
  return(results)
}



