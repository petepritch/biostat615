#### Main function #####
my_pexp <- function(x, rate, lower.tail, log.p) {
  #' copy of the pexp() function
  #'
  #' Does something
  #' @param x a numeric quantile value
  #' @param rate a numeric value of rate parameter
  #' @param lower.tail a logical value
  #' @param log.p: a logical value indicating whether to return the log probability
  warning_message = "Warning, buddy."
  # Checking if input is valid
  if (is.nan(x) || is.nan(rate)) {
    return(x + rate) # will return a NaN sicne NaNs are infectious in R
  }
  # Checking that rate parameter is non-negative
  if (rate <= 0) {
    return(warning_message)
  }
  # Checking that x paramater is non-negative
  if (x < 0) {
    return(0)
  }
  # Multiplying since this is the rate style Exponential distribution
  z = -x * rate

  if (lower.tail == TRUE && log.p == FALSE) {
    result <- -expm1(z)
  }

  else if (lower.tail == TRUE && log.p == TRUE) {

    if (z < -1e-10) {
      result <- log1p(-exp(z))
    }
    else {
      result <- log(x * rate)
    }
  }

  else if (lower.tail == FALSE && log.p == FALSE) {
    result <- exp(z)
  }

  else {
    result <- z
  }

  return(result)
}

print(my_pexp(1e-20, 1, TRUE, TRUE))
