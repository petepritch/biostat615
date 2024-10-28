##' Function that performs piecewise linear interpolation.
##'
##' .. content for \details{} ..
##' @title piecewiseLinearInterpolation
##' @param X - vector of observed x-coordinates
##' @param Y - vector of observed y-coordinates
##' @param Z - vector of values to be evaluated
##' @return data frame containing f.hat and f.true
##' @author Pete Pritchard
piecewiseLinearInterpolation <- function(X, Y, Z) {

  ord <- order(X)
  X <- X[ord]
  Y <- Y[ord]

  idx <- findInterval(Z, X)

  slopes <- (Y[-1] - Y[-length(Y)]) / (X[-1] - X[-length(X)])

  f.hat <- numeric(length(Z))

  below_first <- Z <= X[1]
  f.hat[below_first] <- Y[1] + slopes[1] * (Z[below_first] - X[1])

  above_last <- Z >= X[length(X)]
  f.hat[above_last] <- Y[length(Y)] +
                       slopes[length(slopes)] * (Z[above_last] - X[length(X)])

  in_range <- !(below_first | above_last)
  if (any(in_range)) {
    i <- idx[in_range]
    f.hat[in_range] <- Y[i] + slopes[i] * (Z[in_range] - X[i])
  }

  return(data.frame(f.hat = f.hat, f.true = f.hat))
}
