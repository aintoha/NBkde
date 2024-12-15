fit.classKDE1 <- function(object, newdata, H, ...) {



  n_x = ncol(traindata) # no. of column in test data

  n_variable = ncol(as.matrix(traindata[, -n_x]))  # no. of features

  n_test = nrow(testdata) # no. of instances in testdata

  n_train = nrow(traindata) # no. of instances in traindata

  sort_traindata = dplyr::arrange(traindata, traindata[,n_x])

  n_class = length(unique(traindata[,n_x])) # no. of class

  class_name = unique(sort_traindata[, n_x]) #name of the class

  # setting the prior => prior is equal to the weight of the
  # each class (probability)
  prior = rep(NA, n_class)
  for (k in 1:n_class) {

    class = noquote(as.character(unique(traindata[,n_x])))
    prior[k] = length(which(traindata[, n_x]  == class[k]))/n_train

  }

  length_prior = length(prior) # no. of prior

  if (length_prior != n_class) {

    stop("prior must have the same length as the number or class")

  }

  if (sum(prior) != 1) {

    stop("sum of prior must equal to 1")

  }


  if (is.null(testdata)) {

    testdata = traindata # the covariates

  } else if (!is.matrix(testdata)) {

    testdata = as.matrix(testdata, ncol = n_variable, rownames.force = FALSE)

  } else {

    stop("testdata must be a matrix or data frame")

  }

  if (is.null(traindata)) {

    stop("No train data")

  } else if (is.matrix(traindata) | is.data.frame(traindata)) {

    traindata = as.data.frame(traindata)

  } else {

    stop("traindata must be a matrix or data frame")

  }

  if (!is.list(H) | !is.array(H)){

    H_class = replicate(n_class, H, simplify = "array")

  }


  for (i in 1:n_class)  {

    if(!is.matrix(H_class[[i]])){

      stop ("H must be a matrix")

    } else if (ncol(H_class[[i]]) != n_variable) {

      stop("H must be in same dimension as the training data")

    } else if (ncol(H[[i]]) != nrow(H[[i]])) {

      stop("H must be a square martix")

    }
  }

  # create a subset of
  subset_traindata = subset.data(sort_traindata)

  # data = list()
  joint_pdf = matrix(NA, ncol = n_class, nrow = n_test)

  for(k in 1:n_class) {

    sample_data = subset_traindata[[k]][, -n_x]
    joint_pdf[, k] = m2.kde(data= as.matrix(sample_data, ncol = n_variable, rownames.force=FALSE),
                            eval.points = testdata, H = H[[k]])

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

  colnames(p_class) <- class_name

  h_matrix = H
  # no. of column is number of class
  # no. of row is number of variables
  # colnames(h_matrix) <- names(traindata[,-n_x])
  # rownames(h_matrix) <- names(traindata[,-n_x])

  result = list("posterior probability" = p_class,
                "prior probability" = prior,
                "bandwidth"= h_matrix)
  return(result)

}
