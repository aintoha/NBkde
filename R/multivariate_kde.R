
# Description: This function calculates/ computes the multivariate kde for p > 1 using
#              Gaussian kernel. users need to input/ specify own matrix of bandwidth.
#              This multivariate KDE is a simplified version in which the off-diagonal is 0

# Usage:  m2.kde(data, eval.point, H

# Arguments:
# data: a matrix n x p (p > 1), containing numerical elements
# eval.point: a matrix m x p (p > 1), containing numerical elements at which the pdf
#        is evalautd at
# H: a p x p matrix of bandwidth (variance-covariance)

# Values: a list of joint pdf of length m

# Example:
# data <- iris[1:5, 1:2]
# data <- as.matrix(data, rownames.force=FALSE)
#
# eval.points <- iris[6:8, 1:2]
# eval.points <- as.matrix(eval.points, rownames.force=FALSE)
#
# H <- cov(iris[,1:2])
# # H <- diag(H)
# # H <- diag(H)
# multivariate.kde(X=data, eval.points=eval.points, H=H)

# Exampl 2
# data <- c(1:6)
# data <- matrix(data, ncol=2)
#
# eval.point <- c(0.5, 0.6, 0.7, 0.8)
# eval.point <- matrix(eval.point, nrow =2)
# H <- matrix(c(2, 1, 1, 3), nrow =2)
# multivariate.kde(data, eval.point, H)


# compare with ks::kde and sm::sm.density (1 <= p <= 2)
# ks::kde(x = data, H = H, eval.points = eval.point)$estimate
# sm::sm.density(x=data, h=H, eval.points = eval.point)


multivariate.kde <- function(X, eval.points, H, kernel = "Gaussian"){

  nx = nrow(X)
  p = ncol(X)
  ny = nrow(eval.points)

  if (is.matrix(X)) {

    X = X

  } else  if (is.data.frame(X)) {

    X = as.matrix(X, ncol = p)

  } else {

    stop ("X should be a matrix or data frame")

  }

  if (is.matrix(eval.points)) {

    eval.points = eval.points

  } else if (is.data.frame(eval.points)) {

    eval.points = as.matrix(eval.points, ncol = p)

  } else {

    stop("eval.points must be a matrix or a dataframe")
  }

  if (ncol(eval.points) != ncol(X)) {

    stop("X and eval.points should be in same dimension")

  }

  if (ncol(H) != ncol(eval.points) | ncol(H) != ncol(eval.points)) {

    stop("H should be in same dimension as X and eval.points")

  }

  norm <- ((2 *pi)^(p/2)) * nx * det(H)^(1/2)

  m.pdf <- rep(NA, ny)
  dif.mat.H <- matrix(NA, nrow = ny,ncol = nx)

  for(i in 1:ny){

    dif.mat <- apply(X, 1, function (x,y) (y-x), y = eval.points[i,])

    for (j in 1:nx) {

      dif.mat.H[i,j] <- t(dif.mat)[j,] %*% solve(H) %*% (dif.mat)[,j]

      m.pdf <- rowSums(exp(-1/2 * dif.mat.H)) / norm

    }

  }

  result = list("PDF" = m.pdf, "bandwidth" = H)
  return(result)

}


