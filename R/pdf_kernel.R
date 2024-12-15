#' Probability density function (pdf) for any kernel function
#'
#' @description
#' This function computes the probability density function (pdf) for kernel
#'
#' @param x a vector/ numeric the points in which we want to estimate the probability
#' @param data a vector the windows
#' @param bandwidth a vector/ numeric of bandwidth. If one bandwidth is given to estimate the pdf of vector
#'     the bandwidth will be duplicated. Default value is 1
#' @param kernel_name a string, default is "gaussian"
#'
#' @returns pdf: a vector/ numerical values of the estimated PDF
#' @returns x: the values of points
#' @returns bandwidth: a numerical value
#' @examples
#' x = c(0.11);
#' data = c(0.2, 0.3);
#' bandwidth = 0.1;
#' pdf_kernel(x, data, bandwidth, kernel_name = "gaussian");
#' @export


pdf_kernel <- function(x, data, bandwidth = 1, kernel_name = "gaussian") {

  if(is.na(bandwidth)){

    h = 1

  } else if (class(bandwidth) != "numeric") {

    stop("bandwidth must be a numeric value")

  } else {

    h = bandwidth

  }

  if (!is.numeric(x) | !is.vector(x)) {

    stop("x must be a numeric or a vector")

  }

  n_x = length(x)

  # if (n_bw != n_x) {
  #
  #   if (n_h == 1) {
  #
  #     vec_h = rep(h, n_x)
  #
  #   } else {stop("invalid length of h")}
  #
  # }

  x1 = sapply(x, function(x) (x - data) / h)

  kernel_update = noquote(paste0(kernel_name, ".kernel"))

  if (ncol(x1) == 1) {

    pdf_value = mean(get(kernel_update)(x1) / h)

  } else {

    pdf_value = colMeans(apply(x1, 1,  function(x) get(kernel_update)(x) / h))

  }

  results = structure(list("x" = x, "data" = data, "bandwidth" = h,
                           "kernel" = noquote(kernel_name), "pdf" = pdf_value))
  return(results)

}
