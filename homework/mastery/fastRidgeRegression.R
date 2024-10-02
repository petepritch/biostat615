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
  if (X_dim[1] > X_dim[2]) { # Cholesky
    # Compute A
    A <- t(X) %*% X + lambda * diag(X_dim[2])
    # Compute B
    b <- t(X) %*% Y
    # Decompose A
    U <- chol(A)
    # Foward substitution
    z <- forwardsolve(t(U), b)
    # Backward substitution andstore betas
    betas <- backsolve(U, z)

  } else { # Some other way
    # Compute X transpose X
    K <- X %*% t(X)
    # Solve for alpha
    alpha <- solve(K + lambda * diag(X_dim[1]), Y)
    # store betas
    betas <- t(X) %*% alpha
  }
  # Round betas to nearest int
  betas <- round(betas)
  # Indices of non-zero betas
  zero_index <- which(betas != 0)
  # Return empty df if all zeros
  if (length(zero_index) == 0) {
    return(data.frame(index = integer(0), beta = integer(0)))
  }
  # Result dataframe
  df <- data.frame(
    index = zero_index,
    beta = betas[zero_index]
  )

  return(df)
}
