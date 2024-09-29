#### Main Function #####
#' Computes the predictive probability of a fitted five-category logistic regression model
#'
#'
#'@param x - numeric vector of length p
#'@return vector of five probability values rounded to 8 digits
#'
predProb <- function(x) {

  ### Initialize variables here ###
  p = length(x)                                      # length of input vector
  y = 2^-(1:5)                                       # intercepts
  b_mat = sapply(1:p, function(j) {2^abs(1:5 - j)})  # beta matrix
  #################################
  # Now that we have everything we need to work with,
  # we will calculate and return the probabilities.

  # Linear combination of intercepts and beta matrix
  linear_comb = y + b_mat %*% x
  # Log-sum-exp trick
  max_comb = max(linear_comb)
  exp_linear_comb = exp(linear_comb - max_comb)
  # Divide elements by sum to get associated probabilities (normalize)
  probs = exp_linear_comb / sum(exp_linear_comb)
  # Return probabilities rounded to the 8th digit
  return(probs)
}
