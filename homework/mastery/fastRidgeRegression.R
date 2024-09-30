#' Function that compuetes the integer regression coefficient estimates given X, Y, and lambda.
#'
#'
#' @param X - n x p design matrix
#' @param Y - n x 1 vector of response variables
#' @param lambda - tuning parameter
#' @return Data frame containing two variables - index and beta. The variable index
#'         contains the (1-based) indices of the non-zero regression estimates. The
#'         variable beta contains the corresponding nonzero integer value regression
#'         estimates for each index appears in the same row. If all the regression
#'         coefficeint estiamtes are zero, then there will be no row in the returned
#'         data frame.
fastRidgeRegression <- function(X, Y, lambda) {
  # Dimension of design matrix
  X_dim <- dim(X)
  if (X_dim[1] > p) { # Cholesky
    # Compute A
    A <- crossprod(X) + crossprod(lambda, diag(X_dim[2]))
    # Compute B
    b <- crossprod(X, y)
    # Decompose A
    U <- chol(A)
    # Foward substitution
    z <- fowardsolve(U, A, upper.tri=TRUE, transpose=TRUE)
    # Backward substitution and return result
    return(backsolve(U, z))
  } else { # Some other way

    K <- X %*% t(T)

    alpha <- solve(K + crossprod(lambda, diag(X_dim[2])), y)

    beta <- t(X) %*% alpha
  }

  return()
}
