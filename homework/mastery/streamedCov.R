### Main function ###
#' streamedCov (x, y): calculate covariance between vectors accurately while
#'     streaming. The function must read each element of x and y
#'     without reusing them after an iteration .
#'
#' @param x - A numeric vector
#' @param y - Another numeric vector
#' @return covariance between x and y (with n -1 as denominator )
streamedCov <- function(x, y) {
  # ensure that two input vectors are the same length
  stopifnot (length (x) == length (y))
  #################################
  ### DEFINE NEW VARIABLES HERE ###
  x_bar = 0  # mean of y
  y_bar = 0  # mean of x
  n = 0      # counter
  cov = 0    # covariance of x and y sum
  #################################
  #################################
  for(i in 1:length(x)) {

    xi = x[i] # ith element of x vector
    yi = y[i] # ith element of y vector

    n = i     # increase count

    x_diff = xi - x_bar # difference between streamed mean
    y_diff = yi - y_bar # difference between streamed mean

    ### Uncomment for debug
    # print(paste("Iteration:", i))
    # print(paste("xi:", xi, "yi:", yi))
    # print(paste("x_diff:", x_diff, "y_diff", y_diff))
    # print(paste("Current x_bar:", x_bar, "Current y_bar:", y_bar))

    x_bar = x_bar + (x_diff / n) # update streamed mean
    y_bar = y_bar + (y_diff / n) # update streamed mean

    ### Uncomment for debug
    # print(paste("Updated x_bar:", x_bar, "Updated y_bar:", y_bar))

    cov = cov + x_diff * (yi - y_bar) # update streamed cov sum

    ### Uncomment for debug
    # print(paste("Current cov sum:", cov))
  }

  result = cov / (n - 1)

  ### Uncomment for debug
  # print(paste("Streamed Covariance:", result))

  return(result)
}
