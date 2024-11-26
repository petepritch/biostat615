init_gmm <- function(X, nclust) {

  n <- nrow(X)
  d <- ncol(X)

  labels <- sample(1:k, n, replace = TRUE)

  mu <-
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param X
##' @param nclust
##' @param max.iter
##' @param tol
##' @return z vector of cluters estimated using the EM algo
##' @author Pete Pritchard
mnistEM <- function(X, nclust, max.iter, tol) {


}


# Goal: take a nxp matrix (25x785) and the number of clusters k as input
# and return a vector of length n containing the cluster assignment.
# Returned value should be a list, which contains an attribute est, a vector
# of size n that contains the cluster assignment vector.
#
# Steps:
#
# 1. Normalize the data to 0, 1 scale
# 2. Dimensionality reduction
# 3. Clustering with Gaussian Mixture Models
#    - Initialize GMM
#    - Run EM algo to fit GMM


e_step <- function() {

}

m_step <- function() {

}
