##' Kenel Ridge Regression function
##'
##'
##' @title kernelRidgeRegression.R
##' @param df     - dataframe containg four columns: x_train, y_train, x_test, y_test
##' @param lambda - tuning parameter
##' @param rho    - smoothing paramater
##' @param bw     - bandwidth paramater
##' @return       - a numeric value of the predictive mean square errors (PMSE).
##' @author Pete Pritchard
kernelRidgeRegression <- function(df, lambda, rho, bw) {

  # Initialize some variables
  x_train <- as.vector(df$x_train)
  y_train <- as.vector(df$y_train)
  x_test <-  as.vector(df$x_test)
  y_test <-  as.vector(df$y_test)

  #Kernel function
  kernel_func <- function(x) {
    return( exp(-rho * x^2))
  }

  #K <- exp(-rho * (outer(x_train, x_train, "-")^2)) * (abs(outer(x_train, x_train, "-")) <= bw)
  #K_test <- exp(-rho * (outer(x_test, x_train, "-")^2)) * (abs(outer(x_test, x_train, "-")) <= bw)

  dist_mat <- abs(outer(x_train, x_train, "-") <= bw)
  dist_mat_test <- abs(outer(x_test, x_train, "-") <= bw)
  K <- apply(dist_mat, c(1,2), kernel_func)
  K_test <- apply(dist_mat, c(1,2), kernel_func)

  #K <- Matrix(K, sparse = TRUE)
  #K_test <- Matrix(K_test, sparse = TRUE)


  # Kernel matrix - train
  # K <- outer(x_train, x_train, FUN = Vectorize(function(x, x.prime) kernel_func(x, x.prime, rho, bw)))
  # Kernel matrix - test
  # K_test <- outer(x_test, x_train, FUN = Vectorize(function(x_test, x_train) kernel_func(x_test, x_train, rho, bw)))

  # Solve for beta
  L <- chol(K + lambda * diag(nrow(K)))
  beta <- backsolve(L, forwardsolve(t(L), y_train))

  # PREDICTION
  y_hat <- K_test %*% beta

  # PMSE
  pmse <- mean((y_hat - y_test)^2)

  return ( pmse )

}

### TESTING SPACE ###

#set.seed(44)
#
#n_train <- 100
#n_test <- 50
#
#x_train <- rnorm(n_train)
#y_train <- 3 * x_train + rnorm(n_train)
#
#x_test <- rnorm(n_test)
#y_test <- 3 * x_test + rnorm(n_test)
#
#df <- data.frame(
#  x_train = x_train,
#  y_train = y_train,
#  x_test = x_test,
#  y_test = y_test
#)
#
kernelRidgeRegression(df, 0.1, 0.1, 0.1)
