##' Rejection sampling function specified in file 615/files/ex11.pdf
##'
##'
##' @title
##' @param n - number of simulations
##' @param a - real number
##' @param b - real number
##' @return list containing the following attributes:
##'         - attemtpted: The total number of attempted simulations, which
##'                       should be greater than or equal to n.
##'         - accepted:   The total number of accpted simulations, which should be
##'                       equal to n as well as length(values)
##'         - values:     The vector of accepted values. The length should be equivalent to n
##' @author Pete Pritchard
rejectionSampling <- function(n, a, b) {

  # Target function
  f <- function(x) {

    return(exp(-x^2) * x^(a - 1) * (1 - x)^(b - 1))

  }

  # Envelope function
  g <- function(x) {

    return(dbeta(x, a, b))

  }

  # M estimation
  x_vals <- seq(0.001, 0.999, length.out = 1000)
  M <- max(f(x_vals) / g(x_vals))

  # Initialize parameters
  values <- numeric(n)
  attempted <- 0
  accepted <- 0

  # Loop through rejection sampling
  while (accepted < n) {

    batch_size <- max(2 * (n - accepted), 10000)
    Y <- rbeta(batch_size, a, b)
    U <- runif(batch_size)

    acceptance_prob <- f(Y) / (M * g(Y))

    accepted_indices <- which(U < acceptance_prob)
    num_accepted <- length(accepted_indices)

    if (num_accepted > 0) {

      needed_samples <- min(num_accepted, n - accepted)
      values[(accepted + 1):(accepted + needed_samples)] <- Y[accepted_indices[1:needed_samples]]
      accepted <- accepted + needed_samples
    }


    attempted <- attempted + batch_size

  }

  results <- list(
    attempted = attempted,
    accepted = accepted,
    values = values
  )

  return(results)
}

