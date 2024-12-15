#' Cumulative distribution function for any kernel function
#'
#' @param x a vector/ numeric the points in which we want to estimate the probability
#' @param data a vector the windows
#' @param bandwidth a vector/ numeric of bandwidth. If one bandwidth is given to estimate the pdf of vector
#'     the bandwidth will be duplicated. If the length of vector of bandiwdth is not equal to
#'     the length of x, error will come out
#' @param kernel_name a string
#'
#' @return cdf: a vector/ numerical values of the estimated PDF
#' @return x: the values of points
#' @return bandwidth: bandwidth
#' @examples
#' x = c(0.11, 0.3);
#' data = c(0.2, 0.3);
#' bandwidth = 1;
#' cdf.kernel(x, data, bandwidth, kernel_name = "gaussian");
#' @export

cdf.kernel = function(x, data, bandwidth = 1, kernel_name = "gaussian") {

  if(is.na(bandwidth)){

    h = 1

  } else if (!is.numeric(h)) {

    stop("bandwidth must be a numeric value")

    } else {

    h = bandwidth

  }

  if (!is.numeric(x) | !is.vector(x)) {

    stop("x must be a numeric or a vector")

  }

  n_x = length(x)

  x1 = sapply(x, function(x) (x - data) / h)

  kernel_name = noquote(paste0(kernel_name, ".cdf"))

  if (length(x) == 1) {

    cdf_value = mean(get(kernel_name)(x1))

  } else {

    cdf_value = rowMeans(apply(x1, 1,  function(x) get(kernel_name)(x)))

  }

  results = structure(list("x" = x, "data" = data, "bandwidth" = h,
                           "kernel" = kernel_name, "cdf" = cdf_value))
  return(results)

}
