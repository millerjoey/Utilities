# Computes highest size P interval about mode (default) of 
# (normalized or unnormalized) distribution, returns vector 
# with the indices and prints indices with contained probability.
# change "start= " to include 
# ----------------------------------------------------------------

interval.around.mode1D <- function(density, p, use.mode=True) {
  sum <- 0
  i <- 1
  k <- 1
  
  if (use.mode==TRUE) {
    mode <- max(density)
    indexVector <- mode == density
  }
  if (use.mode==FALSE) {
  
  }
  
  index <- (1:length(density))[indexVector]
      
  if (length(index)>1) {print("Two Modes exist; choosing one", quote = F) ; index <- index[1]}

  density <- density/sum(density)
  
  while (sum < p) { # This starts at mode and expands left or right, depending on which has more mass
    sum <- sum(density[(index-i):(index+k)])
    if (density[index-i] <= density[index+k]) {k <- k + 1}
    else {i <- i + 1} 
  }
  print(c("Indices of interval:", index-i, index+k), quote = F)
  print(c("Contains the following probability:", sum), quote = F)
  return(c(index-i, index+k))
}
