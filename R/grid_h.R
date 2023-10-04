# Description: This function is to estimate/ optimize the bandwidth using
#              grid search method

# Arguments:
#  1. kernel: A string. Default kernel is "Gaussian"
#  2. loss: A string (1) logloss; (2) PSL; (3) IBL. Default loss is "logloss"
#  3. min: The minimum value of the grid search. Default value is
#  4. max: The maximum value of the grid search. Default value is
#  5. by:


logloss <- function(h, traindata, testdata){

  n_train <- length(traindata)
  n_test <- length(testdata)
  #h <-log(h)
  norm <- 1/(sqrt(2*pi)* h)
  train_rep <- replicate(n_test, traindata)
  t_train_rep <- t(train_rep)
  test_rep <- replicate(n_train, testdata)
  D <- t_train_rep - test_rep

  kern <- exp(-(D^2)/(2*h^2))*norm
  fx <- apply(kern, 1, mean)
  logfx <- -log(fx)
  loss <- mean(logfx)

  return(loss)

}

#

v.gaussiandensity <- function(x, xtest, logbw){

  nx <- length(x)
  nxtest <- length(xtest)
  xrep <- replicate(nxtest, x)
  txrep <- t(xrep)
  xtestrep <- replicate(nx, xtest)
  D <- txrep - xtestrep
  t.bw <- exp(logbw)
  kern <- lapply(t.bw, function(t.bw, D) exp(-(D^2)/(2*t.bw^2))*(1/(sqrt(2*pi)* t.bw)),
                 D=D)
  fx <- lapply(kern, rowMeans)
  return(fx)


}

#

V.logloss <- function(logh, obsdata, testdata){

  dens <- v.gaussiandensity(x=obsdata, xtest =testdata, logbw=logh)
  logdens <- lapply(dens,  function(dens) -log(dens))
  loss <- lapply(logdens, mean)
  return(loss)

}


