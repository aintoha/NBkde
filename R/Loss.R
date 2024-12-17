#' @description
#' Generic functions for calculating the Continuous Ranked Probability Score,
#' probabilistic squared loss and the Logarithmic Score.


#' @export
crps <- function(y, ...) {
  UseMethod("crps")
}


#' @export
log <- function(y, ...){
  UseMethod("log")
}


#' @export
squared <- function(y, ...){
  UseMethod("squared")
}
