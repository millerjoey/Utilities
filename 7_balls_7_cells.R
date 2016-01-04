## To resolve an argument inspired by a question in Casella Berger
# -------------------------------------------------------------------
# n balls are distributed uniformly into n cells.
# This function simulates a RV, X_j, the number of cells
# containing exactly j balls


pmf <- function(j, nCells, repetitions) {
  n <- nCells
  data_vector <- rep(0, times=repetitions)
# The following generates repetitions indexed by q ...
  for (q in (1:repetitions)) {
    value <- rep(0, times=n)
# ... of a discrete uniform sequence of r.v. called "arrangement"
# (arrangement[i] should be interpreted as the cell which ball i
# lands in) ...
    arrangement <- sample(1:n, size = n, replace = T)
# ... and increments through a table(repetitions), testing whether
# any value of the discrete unif. r.v. has frequency j, and counts
# them via (value[i] <- 1) and sum(value)
    for (i in 1:length(table(arrangement))) {
      if (table(arrangement)[i]==j) (value[i] <- 1)
    }
    data_vector[q] <- sum(value)
  }
# Below normalizes the empirical distribution so it's a pmf.
print(table(data_vector)/repetitions)
}
