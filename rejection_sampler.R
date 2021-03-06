# A silly script if you already have computed all of the values, but
# this is a simple  Rejection Sampling function over a grid (class: 
# matrix) of values from a bivariate density. Creates data frame with 
# n rows and cols ranAlpha and ranBeta which are indices of the grid 
# sampled randomly from the density. Parameter 1 corresponds to the 
# rows of the matrix. x (default 10) prints out every multiple of 
# x between 1 and n so you can check progress. Uncomment code to 
# activate.
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
    if ( matrix[row,col]/Max >= ran ) {       # Randomly choose indices in proportion to the dens of interest under a flat sampling dist of height = max(dens)
      count[i] <- T
      vectorPar1[i] <- row
      vectorPar2[i] <- col
      i=i+1
      # if ( (sum(count)/x)%%1==0 ) {print(sum(count))} # Uncomment line to check progress.
    } 
  }
  return(cbind(vectorPar1, vectorPar2))
}

# Another 1D rejection sampler:

rejectionSampler <- function(n, option = 2000) {
  samples <- NULL
  total <- NULL
  while (length(samples) < n) {
    x <- runif(option) # wanted to vectorize to speed up. Use option of around 2x desired sample size for best efficiency.
    y <- density(x)
    c <- runif(option) # vectorized
    samples <- c(samples,x[y/max > c])
    total <- c(total, y/max>c)
  }
  print(paste("Acceptance Fraction is ", sum(total)/length(total)), quote = F)
  return(samples[1:n])
}





# TODO: add conditional sampling function, compare efficiencies for a few different densities.

conditionalSample <- function(matrix, n) {

}
