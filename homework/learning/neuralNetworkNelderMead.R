### CREDIT ###
# I received help from Matthew Mcanear on the objective function calculation.



#' Multi-dimensional Nelder-Mead
#' @param f - Objective function to minimize
#' @param x0 - initial point to start
#' @param tol - relative error
#' @param max_iter - maximum iteration
#' @return A list containing the following attributes:
#'    * xmin : x-coordinate of minima
#'    * fmin : f(xmin)
#'    * convergence : 0 if convergend, 1 otherwise
#'    * iter : number of iterations
Nelder.Mead <- function(f, x0, tol = 1e-5, max_iter = 10000,...){
  d <- length(x0)   # d:dimension of the simplex
  X <- matrix(x0,nrow=d,ncol=d+1)    # set d+1 simplex points
  X[,-(d+1)] <- X[,-(d+1)] + diag(d) # create a simplex
  Y <- apply(X,2,f,...)   # evaluate function at each vertex

  ## initialize key variables as NULL
  idx_max <- NULL; idx_min <- NULL; idx_2ndmax <- NULL ## extremes
  mid_point <- NULL; tru_line <- NULL ## mid-point and tru-line

  ## Function to update the extremes
  update.extremes <- function(){
    ## initialize the worst, 2nd-worst, and the best points
    ## note that global assignment was used to update the variables
    ## outside the function
    if(Y[1] > Y[2]){
      idx_max <<- 1; idx_min <<- 2; idx_2ndmax <<- 2 ## note: global assignment
    } else{
      idx_max <<- 2; idx_2ndmax <<- 1; idx_min <<- 1 ## note: global assignment
    }
    if(d>1){ ## update the worsr, 2nd-worst, and the best
      for(i in 3:(d+1)){
        if(Y[i] <= Y[idx_min]){  ## update the best point
          idx_min <<- i
        } else if(Y[i] > Y[idx_max]){ ## update the worst and 2nd-worst points
          idx_2ndmax <<- idx_max; idx_max <<- i
        } else if(Y[i] > Y[idx_2ndmax]){ ## update the 2nd-worst point
          idx_2ndmax <<- i
        }
      }
    }
  }

  ## Function to update the mid-point and the tru-line
  ## used before performing reflection/expansion/contraction
  update.mid.point <- function(){
    mid_point <<- apply(X[,-idx_max,drop=FALSE],1,mean)
    tru_line <<- X[,idx_max] - mid_point
  }

  ## Function update the next point (reflection, expansion, contraction)
  ## the worst point is replaced with the newly evaluated point if improved
  update.next.point <- function(step_scale){
    next_point <- mid_point + tru_line*step_scale
    Y_next <- f(next_point,...)   ## evaluate the function value
    if(Y_next < Y[idx_max]){      ## if improved over the worst point
      X[,idx_max] <<- next_point  ## replace the worst point
      Y[idx_max] <<- Y_next       ## replace the function value
      return(TRUE)                ## indicate that a point was replaced
    } else{
      return(FALSE)               ## indicate that no point was replaced
    }
  }

  ## Function for multiple contaction
  contract.simplex <- function(){
    X[,-idx_min] <<- 0.5*(X[,-idx_min] + X[,idx_min])
    Y[-idx_min] <<- apply(X[,-idx_min],2,f,...)
  }

  #########################################
  ## the main part of Nelder-Mead algorithm
  #########################################
  convergence = 1
  for(iter in 1:max_iter){
    update.extremes()  ## update worst, 2nd-worst, and the best point

    ## check convergence by comparing the range of the function values
    if(abs(Y[idx_max]-Y[idx_min]) <= tol*(abs(Y[idx_max]) + abs(Y[idx_min]) +tol)){
      convergence = 0  ## converged
      break            ## break the loop
    }
    update.mid.point() ## update mid-point, tru-line

    update.next.point(-1.0)       ## reflection
    if(Y[idx_max] < Y[idx_min]){  ## if reflection generated a new minima
      update.next.point(-2.0)     ## perform expansion
    } else if(Y[idx_max] >= Y[idx_2ndmax]){
      if(!update.next.point(0.5)){  ## perform 1-d contraction
        contract.simplex()          ## if contraction failed, perform multiple contraction
      }
    }
  } ## repeat until convergence

  return(list(xmin=X[,idx_min],  ## return the minima
              fmin=Y[idx_min],   ## return the function value at the minima
              convergence=convergence, ## return convergence indicator
              iter=iter          ## return the number of iterations
        ))
}

GeLu <- function(x) { return( x * pnorm(x) ) }

neural.net.loss <- function(alpha, Y, X, p){

  n <- length(Y)

  # Components
  w2_bias <- alpha[1]
  alpha_sub <- alpha[-1]
  w2 <- alpha_sub[1:p]
  w1_bias <- alpha_sub[(p+1):(2*p)]
  w1 <- alpha_sub[(2*p+1):(3*p)]

  inner <- cbind(1, X) %*% t(cbind(w1_bias, w1))
  Y_hat <- cbind(1, GeLu(inner)) %*% c(w2_bias, w2)

  mse <- mean((Y_hat - Y)^2)

  return(mse)
}

eval_count <- 0

eval_counter <- function(alpha, df, p) {

  eval_count <<- eval_count + 1

  return(neural.net.loss(alpha, df$Y, df$X, p))
}

neuralNetworkNelderMead <- function(p , df) {

  initial_alphas <- rep(0, 3*p+1)
  eval_count <<- 0
  result <- Nelder.Mead(eval_counter, initial_alphas, tol=1e-5, max_iter=10000, df=df, p=p)

  return(c(result$fmin, result$iter, eval_count))
}

