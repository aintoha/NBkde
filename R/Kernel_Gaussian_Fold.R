#' Calculating the pdf of a fold Gaussian kernel functions
#'
#' @param x A numerical value.
#' @return A numerical.
#' @examples
#' gaussian.fold.kernel(10);
#' @export

gaussian.fold.kernel <- function(x){

  val = ifelse(x >= 0, 1/ (sqrt(2 *pi)) * exp(-0.5 * x^2) + 1/ (sqrt(2 *pi)) * exp(-0.5 * x^2), 0)

  return(val)

}

#' Calculating the mixture pdf of a Fold Gaussian kernel functions
#'
#' @param x A numerical value.
#' @param data A value or a vector.
#' @param h A numerical value for bandwidth .
#' @return A numerical value.
#' @examples
#' gaussian.fold.pdf(x, data, h);
#' @export

a <- function(x){

  return(1)
}

#' Calculate the cdf of a folded Gaussian kernel from 0 to infinity
#'
#' @param x The value of x
#' @return a numerical / vector
#' @examples
#' gaussian.fold.cdf(10);
#' @export


gaussian.fold.cdf <- function(x) {

  val = (2 * (0.5  * (pracma::erf(x / sqrt(2)) + 1)))  - 1

  return(val)

}

#' Calculate the mean (expected) value, E|X|,  of a folded Gaussian kernel from 0 to infinity
#' @param x The value of x
#' @return a numerical / vector
#' @examples
#' gaussian.fold.mean(10);
#' @export

gaussian.fold.mean <- function(x) {

  val =  x * pracma::erf(x / sqrt(2)) + sqrt(2 / pi) * exp(- (x^2) / 2)

  return(val)
}









