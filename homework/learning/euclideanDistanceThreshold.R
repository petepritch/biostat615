##' This function returns the number of column pairs with
##' Euclidian distance less than or equal to the parameter
##' thres from two matrices X and Y with the same number of
##' rows.
##'
##'
##' @title euclidianDistanceThreshold
##' @param X - pxn matrix
##' @param Y - pxm matrix
##' @param thres - Threshold
##' @return int
##' @author Pete Pritchard
euclideanDistanceThreshold <- function(X, Y, thres) {

  distances <- outer(
    1:ncol(X),
    1:ncol(Y),
    function(i, j) {
      sum((X[, i] - Y[, j])^2)
    }
  )

  distances <- as.matrix(distances)
  sums <- colSums(distances)
  count <- sum(column_sums <= thres^2)

  return ( count )

}
