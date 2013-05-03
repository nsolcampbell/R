rm(list = ls())
library(animation)
neighbours <- function(A, i, j) {
  # calculate number of neighbours of A[i,j] that are infected
  # we have to check for the edge of the grid
  nbrs <- 0
  # sum across row i - 1
  if (i > 1) {
    if (j > 1) nbrs <- nbrs + (A[i-1, j-1] == 1)
    nbrs <- nbrs + (A[i-1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i-1, j+1] == 1)
  }
  # sum across row i
  if (j > 1) nbrs <- nbrs + (A[i, j-1] == 1)
  nbrs <- nbrs + (A[i, j] == 1)
  if (j < ncol(A)) nbrs <- nbrs + (A[i, j+1] == 1)
  # sum across row i + 1
  if (i < nrow(A)) {
    if (j > 1) nbrs <- nbrs + (A[i+1, j-1] == 1)
    nbrs <- nbrs + (A[i+1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i+1, j+1] == 1)
  }
  return(nbrs)
}
 
forest.fire.plot <- function(X) {
  # plot infected and removed individuals
  for (i in 1:nrow(X)) {
    for (j in 1:ncol(X)) {
      if (X[i,j] == 1) points(i, j, col = "red", pch = 19)
      else if (X[i,j] == 0) points(i, j, col = "grey", pch = 8)
      else if (X[i,j] == 2) points(i, j, col = "green", pch = 8)
    }
  }
}
 
a <- 0.07
b <- 0.2
X <- matrix(2, 21, 21)
# spark
X[11, 11] <- 1
 
# Build animation
saveGIF({
    ani.options(convert = shQuote('d:/program files/ImageMagick-6.7.6-Q16/convert.exe'))
    for ( num in 1:100) {
  # set up plot
  plot(c(1,nrow(X)), c(1,ncol(X)), type = "n", xlab = "", ylab = "")
  forest.fire.plot(X)
 
  # main loop
  # update
    B <- X
    for (i in 1:nrow(X)) {
      for (j in 1:ncol(X)) {
        if (X[i, j] == 2) {
          if (runif(1) > (1 - a)^neighbours(X, i, j)) {
            B[i, j] <- 1
          }
        } else if (X[i, j] == 1) {
          burning <- TRUE
          if (runif(1) < b) {
            B[i, j] <- 0
          }
        }
      }
    }
    X <- B
    
    # plot
    forest.fire.plot(X)
  }
})
