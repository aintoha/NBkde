#
#
# Quartic.kernel <- function(x) {
#
#
#   val =  ifelse(x > -1 & x < 1, val = 15 / 16 * (1 - x^2)^2, 0)
#
#   return(val)
#
# }
#
# Quartic.cdf <- function(x) {
#
#   n = length(x)
#
#   val = rep(NA)
#
#   for (i in 1:n) {
#
#     if (x[i] <= -1){
#
#       val[i] = 0
#
#     } else if (x[i] >= 1) {
#
#       val[i] = 1
#
#     } else {
#
#       val[i] = (3 * x[i]^5 - 10 * x[i]^3 + 15 * x[i] + 8) / 16
#
#     }
#
#   }
#   return(val)
# }
#
