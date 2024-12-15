# Description: This function classify p-dimension indepent
#              variables (continuous) of binary classes using KDE.

# Usage:predict.classif.multi.kde(object, testdata)

# Arguments:
#-------------
# 1. object: the object of this class
# 2. testdata: a matrix


# Values:
#-------------
# 1. posterior probabability
# 2. predicted class?
# 2. prior probability
# 3. bandwidth:


# Example:
#-------------------------------------------
# X = iris[, c(1:3)]
# Y = iris[,5]
# testdata = matrix(cbind(7.2, 3.4, 5), ncol = 3)
# kernel = "Gaussian"
# object = fit.classif.multi.kde(X, Y)
# object$bandwidth
# pred = predict.classif.multi.kde(object = object, testdata = testdata)
# pred$posterior_probability

# compare with ks::kda
#----------------------------
# prob = ks::kda(x = X,
#                 x.group  = c(rep("setosa", 50), rep("versicolor", 50),
#                              rep("virginica", 50)),
#                 hs = ks::Hns(X))
# prob$H
# pred.ks = predict(prob, x = c(7.2, 3.4, 5))
# pred.ks
# H <- prob$H
# H

predict.classif.multi.kde <- function(object, testdata) {

  X = object$X

  H = object$bandwidth

  Y = noquote(object$Class)

  n_variable =  ncol(X)  # no. of features

  n_test = nrow(testdata) # no. of instances in testdata

  n_X = nrow(X) # no. of instances in traindata

  sort_x = dplyr::arrange(data.frame(X,Y), Y)

  # create a subset of
  subset_x = subset.data(sort_x)

  n_class = length(unique(Y)) # no. of class

  prior = matrix(object$prior_probability, ncol = n_class)

  if (is.null(testdata)) {

    testdata = X

  } else if (!is.matrix(testdata)) {

    testdata = as.matrix(testdata, ncol = n_variable, rownames.force = FALSE)

  } else if (is.matrix(testdata)) {

    testdata = testdata

  } else {

    stop("testdata must be a matrix or data frame")

  }

  if (is.null(X)) {

    stop("No train data")

  } else if (is.matrix(X) | is.data.frame(X)) {

    traindata = as.data.frame(X)

  } else {

    stop("X must be a matrix or data frame")

  }

  # data = list()
  joint_pdf = matrix(NA, ncol = n_class, nrow = n_test)

  for(k in 1:n_class) {

    sample_data = subset_x[[k]][, 1:n_variable]
    joint_pdf[, k] = multivariate.kde(X= sample_data,
                            eval.points = testdata, H = H[[k]])$PDF

  }

  prob_conditional_pdf = matrix(NA, ncol = n_class, nrow = n_test)

  for (k in 1:n_class) {

    prob_conditional_pdf[, k]  =  joint_pdf[, k] *  prior[k]

  }

  sum_prob_x = as.matrix(apply(prob_conditional_pdf, 1, sum), byrow = T, nrow = n_test)

  p_class = matrix(NA, nrow = n_test, ncol = n_class)

  for (k in 1:n_class) {

    for (i in 1:n_test) {

      p_class[i,k ] = prob_conditional_pdf[i ,k]/sum_prob_x[i,]

    }
  }

  p_class = as.data.frame(p_class)

  class_name = c(unique(sort_x$Y))

  colnames(p_class) <- class_name

  colnames(prior) <- class_name

  names(H) <- class_name

  result = list("clas_name" = class_name,
                "posterior_probability" = p_class,
                "prior_probability" = prior,
                "bandwidth"= H)
  return(result)

}
