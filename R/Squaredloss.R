#' squared_loss
#'
#' @description Compute the probabilistic squared loss
#'              for kernel pdf for a vector using a single bandwidth
#'
#' @param x the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#' @param kernel_name A string for the kernel used. Default is "gaussian"
#'
#' @returns x a vector of x
#' @returns bandwidth
#' @returns squared_loss A vector of CRPS evaluate at x
#' @returns average_loss: The mean of the CRPS
#'
#' @examples
#' set.seed(1001)
#' data <- seq(0.1, 0.5, by = 0.1)
#' x <- c(0.1, 0.2, 0.3,  0.15)
#' bandwidth = 0.1
#' kernel_name = "gaussian"
#' squared_loss(x, data, bandwidth, kernel_name)
#' @export

squared_loss <- function(x, data, bandwidth, kernel_name = "gaussian"){

  loss = -2 * pdf_kernel(x = x, data = data, bandwidth = bandwidth, kernel_name = kernel_name)$pdf +
    + p2norm_pdf(data = data, bandwidth = bandwidth, kernel_name = kernel_name)$p2norm

  mean_loss = mean(loss)

  # loss = colMeans((-2 / (h * N * sqrt(2 * pi))) * exp(- (dif^2)/(2 *h^2))) +
  #   (1 / (2 * N^2 * h * sqrt(pi))) * sum(exp(- (dif.data^2)/(2 *h^4)))
  # mean.loss = mean(loss)
  results = structure(list("x" = x, "kernel" = kernel_name,
                           "bandwidth" = bandwidth,
                           "squared_loss" = loss,
                           "average_loss" = mean_loss))

  return(results)


}
