#' Gradient descent for Gaussian logloss
#'
#' @description Compute the derivative of negative logarithmic score
#'             for Gaussian kernel for a vector using a single bandwidth
#'
#' @param x  the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#'
#' @returns value
#' @examples
#' data <- seq(0.1, 1, by = 0.1)
#' x <- c(0.1, 0.2, 0.3,  0.15)
#' bandwidth = 0.01
#' gradient_descent(x, data, bandwidth)
#' @export
#'
gradient_descent <- function(x, data, bandwidth,  alpha = 0.01, max_iter = 1000, tolerance = 1e-6) {

  h <- bandwidth  # Initial value of h

  for (i in 1:max_iter) {

    gradient <- mean(gradient_logloss(x=x, data= data, bandwidth = h))  # Compute the gradient at the current h

    h_new <- h - alpha * gradient  # Update h

    # Check for convergence (if change is very small)
    if (abs(h_new - h) < tolerance) {
      cat("Converged after", i, "iterations.\n")
      break
    }

    h <- h_new  # Update h for the next iteration
    loss = log_loss(x = x, data = data, bandwidth = h, kernel_name = "gaussian")$average_loss
  }
  results = structure(list("bandwidth" = h, kernel_name = "gaussian",
                           "loss" = loss))
  return(results)  # Return the optimized value of h
}
