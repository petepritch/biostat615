##' This function solved the matrix linear equation AX + XB = C.
##'
##'
##' @title solveMatrixEquation
##' @param A - nxn sparse matrix
##' @param B - nxn sparse matrix
##' @param C - nxn sparse matrix
##' @return sparse matrix, stored in triplet (or COO) format
##' @author Pete Pritchard
solveMatrixEquation <- function(A, B, C) {

  A <- as.matrix(A)
  B <- as.matrix(B)
  C <- as.matrix(C)
  n <- nrow(A) # Dim of matrix A

  if (n <= 100) {
    I <- diag(n) # Identity of dim n
    Cv <- as.vector(C) # Vectorized C
    K <- kronecker(I, A) + kronecker(t(B), I)
    Xv <- qr.solve(K, Cv)
    X <- round(matrix(Xv, nrow = n))
    return( as(X, "TsparseMatrix") )
  }

  schur_A <- Schur(A)
  schur_B <- Schur(B)

  C_trans <- t(schur_A$Q) %*% C %*% schur_B$Q

  Y <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      sum <- C_trans[i,j]
      if (i > 1) {
        sum <- sum - sum(schur_A$T[i,1:(i-1)] * Y[1:(i-1),j])
      }
      if (j > 1) {
        sum <- sum - sum(Y[i,1:(j-1)] * schur_B$T[1:(j-1),j])
      }
      Y[i,j] <- sum / (schur_A$T[i,i] + schur_B$T[j,j])
    }
  }

  X <- schur_A$Q %*% Y %*% t(schur_B$Q)
  X <- round(X)

  return(as(X, "TsparseMatrix"))

}
