# predict ther pdf

# Description: This function to predict the pdf for

# Usage: This function is to predict the probability of the covariates belong to
#        class y ={0,1}. The testdata must be p x n data frame where
#        p is the number of features and is the number of instances.
#        The bandwidth (h) must have the same length as the number of
#        columns (features)
#
# install.packages("distr6")
# install.packages("mlr3proba")
# install.packages("ks")

# Arguments:

# Values:

# Example:
# testdata = data.frame(6.2, 2.8,4.8, 5.0)
# h = c(0.1, 0.2, 0.3, 0.4)
# traindata = iris
# multiple.pdf(traindata = traindata, testdata = testdata, h = h)

# Source code


multiple.pdf = function(traindata, testdata, h){

  n_variable = ncol(testdata)

  if (is.null(testdata)) {

    testdata = traindata #the covariates

  } else {

    if (is.matrix(testdata) | is.data.frame(testdata)) {

      testdata = as.data.frame(testdata)
    }

    else {

      stop("data must be a matrix or data frame")

    }

  }

  n_test = nrow(testdata)

  length_h = length(h)

  if (length_h == 1) {

    h = rep(h, n_variable)
  }

  pdf = matrix(NA, nrow = n_test, ncol = n_variable)

  for (j in 1:n_variable) {

    for (i in 1:n_test) {

      pdf[i, j] = ks::kde(x = as.matrix(traindata)[, j], h = h[j], eval.points = testdata[i, j])$estimate

    }

  }

  return(pdf)

}

