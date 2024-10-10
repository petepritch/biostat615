# Helper
log_sum_exp <- function(log_probs) {
  max_log <- max(log_probs)
  return(max_log + log(sum(exp(log_probs - max_log))))
}
#' Computes the probability mass function of sum of independent Bernoulli random variables
#' using a divide-and-conquer algorithm.
#'
#'
#' @param p - vector of probabilities
#' @return
accurateSumOfBernoulli <- function(p) {
  n <- length(p)

  # Base case: if there are no probabilities, return a PMF of a single value 1
  if (n == 0) {
    return(c(1))
  }

  if (n == 1) {
    return(c(1 - p[1], p[1]))
  }
  # Divide and Conquer
  mid <- floor(n / 2)
  left <- p[1:mid]
  right <- p[(mid + 1):n]

  leftPMF <- accurateSumOfBernoulli(left)
  rightPMF <- accurateSumOfBernoulli(right)

  combined_length <- length(leftPMF) + length(rightPMF) - 1
  combinedPMF <- rep(0, combined_length)

  # Log-space computation to prevent underflow and overflow
  for (i in seq_along(leftPMF)) {
    for (j in seq_along(rightPMF)) {
      # Compute the contribution to the (i+j-1)-th position
      combinedPMF[i + j - 1] <- combinedPMF[i + j - 1] +
        leftPMF[i] * rightPMF[j]
    }
  }

  return(combinedPMF)

}
