% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nb_kde_fit.R
\name{nb_kde.fit}
\alias{nb_kde.fit}
\title{Fit naive bayes for kernel distribution}
\usage{
\method{nb_kde}{fit}(
  x,
  y,
  bandwidth = c("Silverman", "SJ", "logloss", "psl"),
  kernel_name = "gaussian"
)
}
\arguments{
\item{x:}{A matrix of the features (with number of columns is the number of variable and number of rows is the number of observations)}

\item{y:}{A matrix of the target/ label variable (the rows of y must be equal to the rows of x)}

\item{bw:}{must enter out of the 4 options of estimation}

\item{kernel:}{gaussian}
}
\value{
x_name: name of the explanatory/ features variables

y_name: name of the dependent/ target variable

x_data: matrix/ vector of the explanatory/ features variables

y_data: matrix/ vector of the dependent/ target variables

prior: calculated by the proportion of classes

bandwidth
}
\description{
: This function to fit the datasets to naive bayes KDE
}
\examples{
x <- as.matrix(iris[, 1:2])
y <- as.matrix(iris[,5])
fit <- nb_kde(x, y, bw = "Silverman")
fit$bandwidth
predict.nbkde(fit, newdata = as.matrix(iris[c(1, 51), 1:2]), type = "probability")

#compare with the naive bayes
library(naivebayes)
train <- iris[ c(1,2,5)]
test <- iris[c(1,51), 1:2]
nb_kde <- naive_bayes(Species ~ ., data  = train, usekernel = TRUE, kernel = "gaussian")
nb_kde
summary(nb_kde)
plot(nb_kde)
get_cond_dist(nb_kde)
predict(nb_kde, test, type = 'prob')

x <- as.matrix(PlantGrowth[, 1])
y <- as.matrix(PlantGrowth[, 2])
fit <- fit_nbkde(x, y, bw ="Silverman")
fit$bandwidth
predict.nbkde(fit, newdata = as.matrix(PlantGrowth[1:2, 1]), type = "probability")
}
