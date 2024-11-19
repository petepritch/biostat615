### CREDIT ###
# The neural.net.loss function was provided to me by Matt Mcanear, which
# I ultimately used for this assigment as well.
#
# I received help from generative AI on optim function.
# https://chatgpt.com/share/672abbaf-a488-8001-9074-8666ac6674ef
#
# I received help from generative AI on the gradient function.


######################################
########## Helper Functions ##########
######################################


GeLu <- function(x) { return( x * pnorm(x) ) }

GeLu.prime <- function(x) { return( pnorm(x) + x * dnorm(x) )}

neural.net.loss <- function(alphas, Y, X, p) {

  # Initialize params
  w2_bias <- alphas[1]
  alpha_sub <- alphas[-1]
  w2 <- alpha_sub[1:p]
  w1_bias <- alpha_sub[(p+1):(2*p)]
  w1 <- alpha_sub[(2*p+1):(3*p)]

  inner <- cbind(1, X) %*% t(cbind(w1_bias, w1))
  y_hat <- cbind(1, GeLu(inner)) %*% c(w2_bias, w2)

  error <- mean((y_hat - Y)^2)
  return(error)
}

neural.net.gradient <- function(alpha, Y, X, p) {

  # Initialize params
  w2_bias <- alpha[1]
  alpha_sub <- alpha[-1]
  w2 <- alpha_sub[1:p]
  w1_bias <- alpha_sub[(p+1):(2*p)]
  w1 <- alpha_sub[(2*p+1):(3*p)]
  
  inner <- cbind(1, X) %*% t(cbind(w1_bias, w1))
  y_hat <- cbind(1, GeLu(inner)) %*% c(w2_bias, w2)
  error <- as.vector(y_hat - Y)

  grad_w2_bias <- mean(error)
  grad_w2 <- colMeans(sweep(GeLu(inner), 1, error, "*"))

  GeLu_inner_deriv <- GeLu.prime(inner)
  delta_hidden <- GeLu_inner_deriv * (error %*% t(w2))

  grad_w1_bias <- colMeans(delta_hidden)
  grad_w1 <- colMeans(delta_hidden * X)

  gradient <- c(grad_w2_bias, grad_w2, grad_w1_bias, grad_w1)

  return(gradient)

}

######################################
########### Main Function ############
######################################

neuralNetworkLBFGSB <- function(p, df) {

  X <- df$X
  Y <- df$Y

  init_alpha <- runif(3 * p + 1, min = -1, max = 1)

  lb <- rep(-10, length(init_alpha))
  ub <- rep(10, length(init_alpha))

  result <- optim(
    par = init_alpha,
    fn = neural.net.loss,
    gr = neural.net.gradient,
    method = 'L-BFGS',
    lower = lb,
    upper = ub,
    control = list(maxit = 1000),
    Y=Y,
    X=X,
    p=p
  )

  return(result)
}
