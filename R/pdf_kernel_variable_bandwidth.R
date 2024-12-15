#' Probability density kernel function (pdf)
#'
#' @param x a vector/ numeric the points in which we want to estimate the probability
#' @param data a vector the windows
#' @param bandwidth a vector/ numeric of bandwidth. If one bandwidth is given to estimate the pdf of vector
#'     the bandwidth will be duplicated to the number of vector in. If the length of vector of bandwidth is not equal to
#'     the length of x, error will come out
#' @param kernel_name a string
#'
#' @return pdf: a vector/ numerical values of the estimated PDF
#' @return x: the values of points
#' @return bandwidth: a numerical / vector value bandwidth
#' @examples
#' x = c(0.11, 0.3);
#' data = c(0.2, 0.3);
#' bandwidth = c(0.5, 0.4);
#' pdf.kde(x, data, bandwidth, kernel_name = "gaussian");
#' @export


pdf.kde <- function(x, data, bandwidth, kernel_name) {

  h = bandwidth

  if (!is.numeric(x) | !is.vector(x)) {

    stop("x must be a numeric or a vector")

  }

  if (!is.numeric(h)) {

    stop("bandwidth must be a numeric")

  }

  n_x = length(x)

  if (n_h != n_x) {

    if (n_h == 1) {

      h = rep(h, n_x)

    } else {

      stop("invalid length of h")

      }

  }

  data = sapply(x, function(x) (x - data) / h)

  kernel_update = noquote(paste0(kernel_name, ".kernel"))

  if (length(x) == 1) {

    pdf_value = mean(get(kernel_update)(data))

  } else {

    pdf_value = rowMeans(apply(data, 1,  function(x) get(kernel_update)(x)))

  }

  results = structure(list("x" = x, "data" = data, "bandwidth" = h,
                           "Kernel" = kernel_name, "pdf" = pdf_value))
  return(results)

}
