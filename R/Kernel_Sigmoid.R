


Sigmoid.kernel <- function(x){

  return((2/ pi) + (1 / ( exp(x) + exp(-x))))

}

Sigmoid.cdf <- function(x){

  return((2/ pi) * atan(exp(x)))

}


Sigmoid.p2Norm <- function(x){

  return((4 * x * exp(x)) / (pi^2 * (exp(2 * x) - 1)))

}
