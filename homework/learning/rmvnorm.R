##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param n
##' @param p
##' @param rho
##' @return
##' @author Pete Pritchard
rmvnorm <- function(n, p, rho) {


  l <- (0:(p - 1)) / p

  abs_diff_matrix <- abs(outer(l, l, "-"))

  cos_l <- abs(cos(l))
  cos_matrix <- outer(cos_l, cos_l, "+")

  Sigma <- exp(p * log(rho) * abs_diff_matrix^1.99 - cos_matrix)

  eig <- eigen(Sigma)
  V <- eig$vectors
  D <- diag(sqrt(eig$values))

  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- Z %*% D %*% t(V)

  E_maxXj <- mean(apply(X, 1, max))

  E_normX <- mean(sqrt(rowSums(X^2)))

  Pr_X1X2 <- mean(X[, 1] * X[, 2] > 0.5 * rho)

  result <- c(E_maxXj, E_normX, Pr_X1X2)

  return(result)
}
