

# CRPS
#====================================
#' Continuous rank probability score (crps) for any kernel distribution
#'
#' Compute the negative continuous rank probabilistic score for Gaussian kernel for a vector using a single bandwidth
#' @param x the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#' @param kernel_name A string
#' @return x a vector of x
#' @return kernel a string
#' @return bandwidth a numerical value
#' @return crps A vector of CRPS evaluate at x
#' @return Average CRPS The mean of the CRPS
#' @examples
#' data <- seq(0.1, 0.5, by = 0.1);
#' x <- c(0.1, 0.2, 0.3,  0.15);
#' bandwidth <- 0.1;
#' gaussian.crps (x, data, bandwidth);
#' @export

crps <- function(x, data, bandwidth, kernel_name){ ## simulated observation.

  h = bandwidth

  n = length(data)

  dif = sapply(x, function(y) y- data)

  dif_data = sapply(data, function(x) (x - data))

  kernel_name = noquote(paste0(kernel_name, ".kernel"))

  mean_loss = mean(loss)

  results = structure(list("x" = x, "kernel" = kernel_name,
                           "bandwidth" = h, "CRPS" = loss,
                           "average_loss" = mean_loss))
  return(results)

}


# Gaussian CRPS
#====================================
#' Continuous rank probability score (crps) for Gaussian kernel distribution
#'
#' Compute the negative continuous rank probabilistic score for Gaussian kernel for a vector using a single bandwidth
#' @param x the data to evaluate at (numeric/ vector)
#' @param data the data (numeric/ vector)
#' @param bandwidth A numerical value
#' @return x a vector of x
#' @return kernel a string
#' @return bandwidth a numerical value
#' @return crps A vector of CRPS evaluate at x
#' @return Average CRPS The mean of the CRPS
#' @examples
#' data <- seq(0.1, 0.5, by = 0.1);
#' x <- c(0.1, 0.2, 0.3,  0.15);
#' bandwidth <- 0.1;
#' gaussian.crps (x, data, bandwidth);
#' @export

gaussian.crps <- function(x, data, bandwidth) {

  h = bandwidth

  n = length(data)

  dif = sapply(x, function(y) (y- data) / h)

  dif_data = sapply(data, function(x) (x - data) / h)

  loss = colSums(gaussian.fold.mean(dif) * h) / n -
        (0.5 * h * sqrt(2) * sum(gaussian.fold.mean(dif_data / sqrt(2))) / n^2)

  mean_loss = mean(loss)

  results = structure(list("x" = x, "kernel" = "Gaussian",
                           "bandwidth" = h,
                            "CRPS" = loss, "average_loss" = mean_loss))
  return(results)
}
