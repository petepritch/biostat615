### Main Function###
#' This function fits a polynomial regression model with paramater constraints
#'
#'
#'@param p - positive integer
#'@param y - numeric vector
#'@return numeric vector
constrainedPolynomialRegression <- function(p, y) {

  n <- length(y)      # length of y vector n
  x <- seq(1, n) / n  # X_i

  # Horner's
  horner <- function(x, p) {
    result <- 1
    for (j in p:1) {
      result <- 1 + x * result / j
    }
    return(result)
  }

  X <- sapply(x, horner, p = p)

  sum_X <- sum(X)
  sum_Y <- sum(y)
  sum_XY <- sum(X * y)
  sum_X2 <- sum(X^2)

  # Solving for slope with tansformed X
  numerator <- n * sum_XY - sum_X * sum_Y
  denominator <- n * sum_X2 - sum_X^2
  beta_1 <- numerator / denominator
  beta_0 <- mean(y) - beta_1 * mean(X)


  # Beta vector
  betas <- numeric(p + 1)
  betas[1] <- beta_0
  betas[2] <- beta_1
  for (j in 3:(p + 1)) {
    betas[j] <- betas[j-1] / (j - 1)
  }

  return(betas)
}

print(constrainedPolynomialRegression(3, c(0.5, 1, 1.5)))
