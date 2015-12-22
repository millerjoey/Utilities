# Rejection Sampling Function over a 

rejectionSample <- function(matrix, n) {
  vector <- rep(NA, times = n)
  vectorPar1 <- rep(NA, times = n)
  vectorPar2 <- rep(NA, times = n)
  
    row <- sample(1:dim(matrix)[1], size = n)
    col <- sample(1:dim(matrix)[2], size = n)
    ran <- runif(n, min = 0, max = 1)
    Max <- max(matrix)
    matrix[row,col]/Max >= ran

  ranAlpha <<- vectorPar1[!is.na(vectorPar1)]
  ranBeta <<- vectorPar2[!is.na(vectorPar2)]
  ranSample <- vector
  ranSample <<- ranSample[!is.na(ranSample)]
  print(length(ranSample))
}
