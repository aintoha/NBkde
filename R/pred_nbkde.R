# Description: This function to predict using to naive bayes KDE that was fitted

# Usage: This function is predict the probability and class using naive bayes using KDE.


# Arguments:

# Test: must be a matrix that consist of independent variable and class
#       The number of variables should match the number of rows in object
#

# Values:

# Example


pred_nbke <- function(object, test) {

  if (missing(test)) {

    stop("no test data")

  }

  if (ncol(test) == ncol(object$bandwidth)) {



  }

}
