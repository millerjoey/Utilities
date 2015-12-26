# Rejection Sampling Function over a grid (class: matrix) of values
# from a bivariate density. Creates data frame with n rows and cols 
# ranAlpha and ranBeta which are indices of the grid sampled randomly 
# from the density. Parameter 1 corresponds to the rows of the matrix.
# x (default 10) prints out every multiple of x between 1 and n so you 
# check progress. Uncomment code to activate.
# --------------------------------------------------------------------

rejectionSample <- function(matrix, n, x=10) {

  count <- rep(F, times = n)
  vectorPar1 <- rep(NA, times = n)
  vectorPar2 <- rep(NA, times = n)
  Max <- max(matrix)
  i=1
  while ( sum(count) < n ) {
    row <- sample(1:dim(matrix)[1], size = 1) # Sample an index from the rows
    col <- sample(1:dim(matrix)[2], size = 1) # Sample an index from the columns
    ran <- runif(1)                           # Generate random number so indices are picked in proper proportion
    if ( matrix[row,col]/Max >= ran ) {       # Randomly choose indices in proportion to the dens
      count[i] <- T
      vectorPar1[i] <- row
      vectorPar2[i] <- col
      i=i+1
      # if ( (sum(count)/x)%%1==0 ) {print(sum(count))} # Uncomment line
      # 24 to check progress.
    } 
  }
  return(cbind(vectorPar1, vectorPar2))
}
