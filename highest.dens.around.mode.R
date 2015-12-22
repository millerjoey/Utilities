# Computes highest size P interval about mode of (normalized or
# unnormalized) distribution, returns vector with the indices 
# and prints indices with contained probability.
# ----------------------------------------------------------------

interval.around.mode1D <- function(density, p, mode=max(normalized_data)) {
  sum <- 0
  i <- 1
  k <- 1
  mode <- max(density)
  modeVector <- mode == density
  modeIndex <- (1:length(density))[modeVector]
  
  if (length(modeIndex)>1) {print("Two Modes exist; choosing one", quote = F) ; modeIndex <- modeIndex[1]}

  density <- density/sum(density)
  
  while (sum < p) { # This starts at mode and walks left or right, depending on which has more mass
    sum <- sum(density[(modeIndex-i):(modeIndex+k)])
    if (density[modeIndex-i] <= density[modeIndex+k]) {k <- k + 1}
    else {i <- i + 1} 
  }
  print(c("Indices of interval:", modeIndex-i, modeIndex+k), quote = F)
  print(c("Contains the following probability:", sum), quote = F)
  return(c(modeIndex-i, modeIndex+k))
}
