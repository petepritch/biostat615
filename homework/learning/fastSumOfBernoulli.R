#' Computes the probability mass function of independent Bernoulli Random variables using a
#' divide-and-conquer algorithm within O(n(logn)^2) time complexity.
#'
#' REQUIREMENTS: 1) Must be divide and conquer algorithm that uses recursion. 2) The time complexity
#' must be faster than O(n^2). 3) No statistical functions may be used - you need to omplement the
#' p.m.f. from scratch without relying on existing functions.
#'
#' @param p - input vector representing (p_1,...,.p_n) probabilities
#' @return vector of length n+1
fastSumOfBernoulli <- function(p) {
  # Base case
  n <- length(p)

  # Base case: if there are no probabilities, return a PMF of a single value 1
  if (n == 0) {
    return(c(1))
  }

  # Base case for single probability
  if (n == 1) {
    return(c(1 - p[1], p[1]))
  }

  # Split the probability vector into two halves
  mid <- floor(n / 2)
  left <- p[1:mid]
  right <- p[(mid + 1):n]

  # Recursively compute the PMF for each half
  leftPMF <- fastSumOfBernoulli(left)
  rightPMF <- fastSumOfBernoulli(right)

  # Convolve the PMFs of the left and right halves
  combinedPMF <- convolve(leftPMF, rev(rightPMF), type = "open")
  combinedPMF <- combinedPMF[1:(n + 1)]

  # Ensure probabilities are within the range [0, 1]
  combinedPMF[combinedPMF < 0] <- 0
  combinedPMF[combinedPMF > 1] <- 1

  return(combinedPMF)
}
