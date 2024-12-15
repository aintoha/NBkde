# Description: This function to fit the datasets to naive bayes KDE

# Usage: fit <- fit_nbkde(x, y, bw = "Silverman", kernel = "gaussian")

# Arguments:
# 1. x: A matrix of the features (with number of columns is the number of variable and number of rows is the number of observations)
# 2. y: A matrix of the target/ label variable (the rows of y must be equal to the rows of x)
# 3. bw: must enter out of the 4 options of estimation
# 4. kernel: gaussian

# Values: A list of the following:
# 1. x
# 2. y
# 3. prior: calculated by the proportion of classes
# 4. bandwidth


# Example

# x <- as.matrix(iris[, 1:2])
# y <- as.matrix(iris[,5])
# fit <- fit_nbkde(x, y, bw = "Silverman")
# fit$bandwidth
# predict.nbkde(fit, newdata = as.matrix(iris[c(1, 51), 1:2]), type = "probability")

#compare with the naive bayes
# library(naivebayes)
# train <- iris[ c(1,2,5)]
# test <- iris[c(1,51), 1:2]
# nb_kde <- naive_bayes(Species ~ ., data  = train, usekernel = TRUE, kernel = "gaussian")
# nb_kde
# summary(nb_kde)
# plot(nb_kde)
# get_cond_dist(nb_kde)
# predict(nb_kde, test, type = 'prob')

# x <- as.matrix(PlantGrowth[, 1])
# y <- as.matrix(PlantGrowth[, 2])
# fit <- fit_nbkde(x, y, bw ="Silverman")
# fit$bandwidth
# predict.nbkde(fit, newdata = as.matrix(PlantGrowth[1:2, 1]), type = "probability")

fit_nbkde = function(x, y, bw = c("Silverman", "SJ", "logloss", "psl") , kernel = "gaussian") {

  if (!is.matrix(x)){

    stop("x must be a matrix")
  }

  if(!is.matrix(y)) {

    stop("y must be matrix")

  }

  if (nrow(x) != nrow(y)) {

    stop("wrong dimensions")

  }

  n_features = ncol(x) # no. of column in x = number of variables

  n_class = length(unique(y)) # no. of class

  if (n_class == 1){

    stop("Number of class must be 2 or more")
  }

  class_name = unique(y) #name of the class

  feature_name = colnames(x)

  prior = rep(NA, n_class)

  for (k in 1:n_class) {

    class = noquote(as.character(unique(y)))
    prior[k] = length(which(y == class[k]))/length(y)

  }

  length_prior = length(prior) # no. of prior

  if (length_prior != n_class) {

    stop("prior must have the same length as the number or class")

  }

  if (sum(prior) != 1) {

    stop("sum of prior must equal to 1")

  }

  data <- data.frame(x, y)
  #combine the x and y so that we can subset the date based on the class

  subset_data = subset.data(data)

  h = matrix(NA, ncol = n_class, nrow = n_features)
  # an empty matrix to save the estimated bandwidth

  if (is.character(bw)) {

    if (bw == "Silverman") {

      for (j in 1:n_class) {

        for (i in 1:n_features) {

          h[i, j] = bw.nrd0(subset_data[[j]][,i])
        }
      }
    } else if (bw == "SJ") {

      for (j in 1:n_class) {

        for (i in 1:n_features) {

          h[i, j] = bw.SJ(subset_data[[j]][,i])

        }
      }
    }
  } else if (is.na(bw)) {

    ind = list()
    train = list()
    test = list()

    for (k in 1:n_class) {

      ind[[k]] <- sample(2, nrow(subset_data[[k]]),
                         replace = T, prob = c(0.8, 0.2))

      train[[k]] <- subset_data[[k]][ind[[k]] == 1,]
      test[[k]] <- subset_data[[k]][ind[[k]] == 2,]

      for (i in 1:n_features) {

        h[i,k] = grid.loss(train = train[[k]][, i], test = test[[k]][,i], kernel = kernel)

      }
    }
  } else if (is.numeric(bw)) {

    if (length(bw) != n_features * n_class) {

      stop("Number of bandwidth is invalid")

    } else {

      h= matrix(bw, byrow = T, ncol = n_class)

    }
  } else if (is.matrix(bw)) {

    if (nrow(bw) != n_features) {

      stop("Number of row for bandwidth is invalid. Number of column must equal to number of class")

    }

    if (ncol(bw) != n_class) {

      h = matrix(rep(bw, n_class), ncol= n_class)

    }

  } else if (is.matrix(bw)) {

    if (nrow(bw) == n_features) {

      if (ncol(bw) == n_class) {

        h =bw
      }
    }
  }

  bw_matrix = h
  # no. of column is number of class
  # no. of row is number of variables
  colnames(bw_matrix) <- class_name
  rownames(bw_matrix) <- feature_name

  prior_matrix <- matrix(prior, nrow = 1)
  colnames(prior_matrix) <- class_name

  structure(list(data = list(x = x, y = y), prior = prior_matrix,
                 bandwidth= bw_matrix, kernel = "gaussian"))

}

#-----------------------------------------------------------------------------

# Description: this model is used to predict the probability using the fitted model

# usage:


# Arguments:
# 1. object: fitted model
# 2. newdata: A test data that consist of only the features/ independent variable

# Values:

predict.nbkde <- function(object, newdata = NULL, type = c("class", "probability")) {

  if (is.null(newdata)){

    newdata = as.matrix(object$data$x) # if newdata is NULL then use the data in object

  } else if (!is.matrix(newdata)) {

    stop("newdata must a matrix")

  }

  subset_traindata = subset.data(data.frame(object$data$x, object$data$y))
  # subset the combined train data from the object

  n_variable = ncol(data.frame(object$data$x, object$data$y))
  # the number of variables (features plus label)

  n_class = length(object$prior)
  #number of class

  class_name = unique(object$data$y)
  #name of the class

  n_features = ncol(object$data$x)
  #number of variables

  data = list()
  pdf = list()

  # compute the pdf for each test row for each target class
  for(k in 1:n_class) {

    data[[k]]= subset_traindata[[k]]
    pdf [[k]] = multiple.pdf(traindata = data[[k]][,-n_variable ],
                             testdata = newdata, h = object$bandwidth[,k])

  }
  # use the train data (object$x) to predict the pdf of the newdata

  # predict likelihood
  #----------------------

  unlist_pdf= list()

  for (k in 1:n_class) {

    unlist_pdf[[k]] = data.frame(unlist(pdf[[k]]))

  }
  # add log

  log_pdf= list()
  sum_pdf = matrix(NA, nrow = nrow(newdata), ncol = n_class)

  for (k in 1:n_class) {

    log_pdf[[k]] = data.frame(log(unlist_pdf[[k]]))
    sum_pdf[,k]  = apply(log_pdf[[k]],1, sum )

  }

  # numerator
  likelihood <- matrix(NA, nrow = nrow(newdata), ncol = n_class)
  for (k in 1:n_class) {
    for (i in 1:nrow(newdata)){
    likelihood[i, k] = sum_pdf[i, k] + log(prior[k])
    }
  }



  colnames(likelihood) <- class_name
  structure(list(Probability =  likelihood))

}
