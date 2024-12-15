# Fit classification using multivariate KDE

# Description:

# Argument:
# 1. Y: Class vector
# 2. X: Matrix if numberical predictors
# 3.


# Values:
# 1. posterior probability
# 2. Prior probabilities
# 3. Class name
# 4. Kernel use
# 5. Bandwidth used
# 6. X (if called)
# 7. Y (if called)

# Example:
# X = iris[,c(1:4)]
# Y = iris[,5]
# eval = cbind(7.2, 3.4, 5, 6)
# kernel = "Gaussian"
# fit.classif.multi.kde (X, Y)


fit.classif.multi.kde <- function(X, Y, eval.points = NULL,
                                  kernel = "Gaussian", H= NULL,...) {

  n_x = nrow(X) # no. of observations/ rows

  n_variable = ncol(X)  # no. of features

  sort_x = dplyr::arrange(cbind(X,Y), Y)

  class_name = unique(Y) #name of the class

  n_class = length(class_name) # no. of class

  if (is.null(X)) {

    stop("No X given")

  } else if (is.matrix(X)) {

    X = X

  } else if (is.data.frame(X)){

    X = as.matrix(X, ncol = n_variable)

  } else {

    stop("X must be a matrix of data frmae")

  }

  if (is.null(eval.points)) {

    eval.points = X # the covariates

  } else if (is.matrix(eval.points)){

    eval.points = eval.points

  } else if (is.data.frame(eval.points)) {

    eval.points = as.matrix(eval.points, ncol = n_variable, rownames.force = FALSE)

  } else {

    stop("eval must be a matrix")

  }

  # no. of instances in testdata
  n_eval = nrow(eval.points)

  # setting the prior => prior is equal to the weight of the
  # each class (probability)
  prior = rep(NA, n_class)
  for (k in 1:n_class) {

    class = noquote(as.character(unique(Y)))
    prior[k] = length(which(Y == class[k]))/n_x

  }

  # define the length of prior
  length_prior = length(prior) # no. of prior

  # check: the length of prior = length of class
  if (length_prior != n_class) {

    stop("prior must have the same length as the number or class")

  }

  # check: the sum of prior must equal to 1
  if (sum(prior) != 1) {

    stop("sum of prior must equal to 1")

  }

  if (kernel == "Gaussian"){

    kernel = "Gaussian"

  } else if (is.null(kernel)){

    kernel == "Gaussian"

  }

  # create a subset of C classes
  subset_x= subset.data(sort_x)

  # make this for each class using for loop

  if(is.null(H)){

    H_rep= rep("NULL", n_class)

  }

  h_list = list()
  for (i in 1:n_class) {

      if (H_rep[i] == "NULL") {

        h_list[[i]] = ks::Hns(subset_x[[i]][, 1:n_variable]) # normal scale bandwidth

      } else if (length(H_rep[i]) == 1) {

        stop ("H must be a matrix")

      } else if (length(H_rep[i]) == length(class)) {

        h_list = diag(H_rep[i])

      } else {

        h_list = H
      }

  }

  # define sample data
  sample_data = list()

  #define a matrix for pdf
  joint_pdf = matrix(NA, ncol = n_class, nrow = n_eval)

  for(k in 1:n_class) {

    sample_data[[k]] = subset_x[[k]][, 1:n_variable]
    joint_pdf[ , k] = multivariate.kde(X= sample_data[[k]],
                            eval.points = eval.points, H = h_list[[k]])$PDF

  }

  prob_conditional_pdf = matrix(NA, ncol = n_class, nrow = n_eval)

  for (k in 1:n_class) {

    prob_conditional_pdf[, k]  =  joint_pdf[, k] *  prior[k]

  }

  sum_prob_x = as.matrix(apply(prob_conditional_pdf, 1, sum), byrow = T, nrow = n_test)

  posterior_prob = matrix(NA, nrow = n_eval, ncol = n_class)

  for (k in 1:n_class) {

    for (i in 1:n_eval) {

      posterior_prob[i,k ] = prob_conditional_pdf[i ,k]/sum_prob_x[i,]

    }
  }

  posterior_prob = as.data.frame(posterior_prob)

  colnames(posterior_prob) <- class_name

  # no. of column is number of class
  # no. of row is number of variables
  # colnames(h_matrix) <- names(traindata[,-n_x])
  # rownames(h_matrix) <- names(traindata[,-n_x])

  result = list("posteriorprobability" = posterior_prob,
                "prior_probability" = prior,
                "bandwidth"= h_list,
                "X" = X,
                "Class" = Y)

  return(result)

}


