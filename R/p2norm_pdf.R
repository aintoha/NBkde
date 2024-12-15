#' L2 - norm of kernel probability density function
#'
#' @description
#' This function computes the L2-norm of the kernel probability density function.
#' \deqn{
#' L_{2} = \int_{-\inf}^{\inf} (f(x))^2 \ dx
#' }
#' where \eqn{f(x) = \frac{1}{nh}\sum_{i=1}^{n} K\left(\frac{x - X_{i}}{h}\right)}
#'
#' @param data a vector the windows
#' @param bandwidth a numeric value of bandwidth. The default value is 1.
#' @param kernel_name a string. The default value is "gaussian"
#'
#' @return data: Vector of data input
#' @return bandwidth: bandwidth
#' @return kernel_name: name of the kernel used
#' @return p2norm: a vector/ numerical values of the estimated PDF
#' @examples
#' x = c(0.11)
#' data = c(0.2, 0.3)
#' bandwidth = 1
#' kernel_name = "gaussian"
#' p2norm_pdf(data = data , bandwidth = bandwidth, kernel_name)
#' @export
#'

p2norm_pdf <- function(data, bandwidth=1, kernel_name = 'gaussian') {

  if(is.na(bandwidth)){

    h = 1

  } else if (!is.numeric(bandwidth)) {

    stop("bandwidth must be a numeric value")

  } else {

    h = bandwidth

  }

  if(missing(data)){

    stop("missing data")

  }

  if (!is.numeric(data) | !is.vector(data)) {

    stop("data must be a numeric or a vector.")

  }

  n = length(data)

  x1 = sapply(data, function(x) (x - data))

  kernel_update = noquote(paste0(kernel_name, ".p2norm"))

  p2norm_value = sum(get(kernel_update)(x1)) / (n^2 * h)

  results = structure(list("data" = data, "bandwidth" = h,
                           "kernel"= kernel_name, "p2norm" = p2norm_value,
                           "class" = "distribution"))

  return(results)

}
