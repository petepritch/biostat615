#' Returns the stationary distribution of a discrete Markov Chain.
#' No functions used outside of base R.
#' Accurate up to 8 significant digits.
#'
#' @param P - transition probability matrix (numeric vector)
#' @return res - numeric vector
stationaryDistribution <- function(P) {
  # I - P
  p <- diag(nrow(P)) - P
  # (I - P) transposed with zero row
  A <- rbind(p, rep(1, ncol(P)))
  # Zero vector with 1 added to last element
  b <- c(rep(0, nrow(P)), 1)
  # Solving the equation
  res <- qr.solve(A, b)
  return(res)
}
