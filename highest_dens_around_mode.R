# Computes highest size P interval about mode of (normalized or 
# unnormalized) distribution, returns vector with the bounds of
# the indices and prints indices with contained probability.
# ----------------------------------------------------------------

# Example:
# > dens <- dnorm(seq(-5, 5, length.out = 1000), 0.95)
# > interval.around.mode1D(dens, 0.95)
# [1] Indices of interval: 399                  791                 
# [1] Contains the following probability: 0.950257756719443                  
# [1] 399 791

interval.around.mode1D <- function(density, p) {
  sum <- 0
  i <- 1
  k <- 1

  mode <- max(density)
  indexVector <- mode == density
  
  index <- (1:length(density))[indexVector]
  
  if (length(index)>1) {print("Two Modes exist; choosing one", quote = F) ; index <- index[1]}
  
  density <- density/sum(density)
  
  while (sum < p) { # This starts at mode and expands left or right, depending on which has more mass
    sum <- sum(density[(index-i):(index+k)])
    if (density[index-i] <= density[index+k]) {k <- k + 1}
    else {i <- i + 1} 
  }
  print(c("Bounds of interval:", index-i, index+k), quote = F)
  print(c("Contains the following probability:", sum), quote = F)
  return(c(index-i, index+k))
}

# ================================================================
# The following computes the highest density region for a 
# distribution (given as a vector of relative probabilities). Use
# cautiously, especially if distribution is MCMC and has many local
# maxima. Returns a vector of indices corresponding to the highest
# p fraction of density.
# ----------------------------------------------------------------
highest.density.region1D <- function(density, p=0.95) {
  
  density <- density/sum(density)
  indices <- 1:length(density)
  
  order <- indices[order(density, decreasing = TRUE)]
  
  sum <- 0
  i <- 0
  
  while (sum < p) {
    i <- i + 1
    sum <- sum(density[(order[1:i])])
    # print(sum)          # Uncomment to monitor progress.
  }
  return(order[1:i])
}

# Example:
# x <- seq(from = -4, to = 4, length.out = 1000)
# dens <- dnorm(x)/sum(dnorm(x))
# 
# output <- highest.density.region1D(dens, p=0.95)
# sum(dens[output])
# [1] 0.9502929
