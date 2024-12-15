
Triweight.kernel <- function(x) {

  val =  ifelse(x > -1 & x < 1, 35 / 32 * (1 - x^2)^3, 0)

  return(val)


}

Triweight.cdf <- function(x) {

  n = length(x)

  val = rep(NA)

  for (i in 1:n) {

    if (x[i] <= -1){

      val[i] = 0

    } else if (x[i] >= 1) {

      val[i] = 1

    } else {

      val[i] = (-5 * x[i]^7 + 21 * x[i]^5 - 35 * x[i]^3 + 35 * x[i] + 16) / 32

    }

  }
  return(val)
}
