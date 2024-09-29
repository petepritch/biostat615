int2BinaryStr <- function(n) {
  #' Function that takes a 32 bit integer
  #' input and returns a string representation
  #' of the binary representation of
  #' the input.
  #'
  #' The functions should return an NA if
  #' the input is not a 32-bit integer.

  # Check if numeric, if integer and within 32-bit range
  if (!is.numeric(n) || floor(n) != n || n < -2^31 || n > 2^31 - 1) {
    return(NA)
  }

  binary_str <- paste(rev(as.integer(intToBits(as.integer(n)))), collapse="")

  return(binary_str)
}
