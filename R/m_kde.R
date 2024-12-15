
# Description: This function calculates/ computes the multivariate kde for p > 1 using
#              Gaussian kernel. users need to input/ specify own matrix of bandwidth.
#              This multivariate KDE is a simplified version in which the off-diagonal is 0
# Usage:  m.kde(vec.x, vec.y, H

# Arguments:
# vec.x: a matrix n x p (p > 1), containing numerical elements
# vec.y: a matrix m x p (p > 1), containing numerical elements at which the pdf
#        is evalautd at
# H: a p x p matrix of bandwidth (variance-covariance)

# Values: a list of joint pdf of length m

# Example:
# vec.x <- iris[1:5, 1:2]
# vec.x <- as.matrix(vec.x, rownames.force=FALSE)
#
# vec.y <- iris[6:10, 1:2]
# vec.y <- as.matrix(vec.y, rownames.force=FALSE)
#
# H <- cov(iris[,1:2])
# H <- diag(H)
# H <- diag(H)
# m.kde(vec.x, vec.y, H)

# compare with ks::kde and sm::sm.density (1 <= p <= 2)
# ks::kde(x = vec.x, H = H, eval.points = vec.y)$estimate
# sm::sm.density(x=vec.x, h=H, eval.points = vec.y)


m.kde <- function(vecx.x, vec.y, H){

  nx = nrow(vec.x)
  p = ncol(vec.x)
  ny = nrow(vec.y)

  if (!is.matrix(vec.x)) {

    stop("vec.x must be a matrix")

  }

  if (!is.matrix(vec.y)) {

    stop("vec.y must be a matrix")

  }

  if (ncol(vec.y) != ncol(vec.x)) {

    stop("vec.x and vec.y should be in same dimension")

  }

  if (ncol(H) != ncol(vec.y) | ncol(H) != ncol(vec.y)) {

    stop("H should be in same dimension as vec.x and vec.y")

  }

  k.dif.mat.H <- rep(NA, ny)

  for(i in 1:ny){

  dif.mat <- apply(vec.x, 1, function (x,y) (y-x), y = vec.y[i,])

  dif.mat.H <- exp(-1/2 * t(dif.mat) %*% solve(H) %*% (dif.mat))

  k.dif.mat.H[i] <- sum(diag(dif.mat.H)) / (((2 *pi)^(p/2)) * nx * det(H)^(1/2))

  }

  result = structure(k.dif.mat.H)
  return(result)

}


