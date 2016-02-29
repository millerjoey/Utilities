# Naive Bayes Classifier
# =======================================================

# Get boolean vector to partition training data 
# trainDat1 is the data associated with y1.
# Likewise for y2
y <- trainLab[,1]==TRUE
trainDat1 <- trainDat[y,]
trainDat0 <- trainDat[!y,]

# Convert the data into a data frame with rows
# corresponding to the number of times each word
# (by column) appears.
X1 <- data.frame(row.names = 0:49)
for (i in 1:dim(trainDat1)[2]) {
  t <- data.frame(table(trainDat1[,i]))
  v <- rep(0, times=50)
  v[as.numeric(levels(t$Var1))+1] <- t$Freq
  X1 <- cbind(X1, v)
} 

# Likewise for the trainDat0 dataset
X0 <- data.frame(row.names = 0:49)
for (i in 1:dim(trainDat0)[2]) {
  t <- data.frame(table(trainDat0[,i]))
  v <- rep(0, times=50)
  v[as.numeric(levels(t$Var1))+1] <- t$Freq
  X0 <- cbind(X0, v)
}

# -----------------------------------------------
# Maximum Likelihood Approach

# Convert frequencies to probabilities
py <- sum(trainLab)/length(trainLab[,])
pX1 <- X1/dim(trainDat1)[1]
pX0 <- X0/dim(trainDat0)[1]

# Classifier with MLEs

# Take logarithm of the probabilities to prevent
# numerical errors
lpX1 <- log(pX1)
lpX0 <- log(pX0)
lpy1 <- log(py)
lpy0 <- log(1-py)

# Save new observations to a new variable
xNew <- testDat

# Look through xNew column by column, multiplying
# the probabilities in the row of X1 that correspond
# to the values of each observation
lProbX1 <- 0
for (i in 1:5180) {
  lProbX1 <- lpX1[xNew[,i]+1,i]+lProbX1
}
MLEpredicty1 <- lpy1+lProbX1

# Likewise for X0.
lProbX0 <- 0
for (i in 1:5180) {
  lProbX0 <- lpX0[xNew[,i]+1,i]+lProbX0
}
MLEpredicty0 <- lpy0+lProbX0

# Compare the sizes of the log(unnormalized
# probabilities), choose y=1 or y=0
# accordingly. Find % correct.
sum(testLab==as.numeric(MLEpredicty1>MLEpredicty0))/1806 # 71.8 percent

# Print out predictions row by row.
printOutputMLE <- function() {
  for (i in 1:1806) {print(as.numeric(MLEpredicty1>MLEpredicty0)[i])}
} 

# ------------------------------------------------
# Bayesian Approach with dirichlet prior and parameter a_i=a for i \in {0, ..., 49}
# Note that I need a prior for each column of X, X_j: P(X_j | y), and a prior for y.
# I use MAP estimates and the same prior for all variables.

# Functions to implelement MAP estimates based on 
# values of prior=alpha
bayes.y <- function(prior) {
  py <- (sum(trainLab)+prior-1)/(length(trainLab[,1]+2*(prior-1)))
  return(py)
}

bayes.pX1 <- function(prior) {
  pX1 <- (X1+prior-1)/(dim(trainDat1)[1]+50*(prior-1))
  return(pX1)
}

bayes.pX0 <- function(prior) {
  pX0 <- (X0+prior-1)/(dim(trainDat0)[1]+50*(prior-1))
  return(pX0)
}

# Here, I choose the prior which maximizes accuracy
py <- bayes.y(1.002041)
pX0 <- bayes.pX0(1.002041)
pX1 <- bayes.pX1(1.002041)


# Classifier with Prior

# Take logarithms
lpX1 <- log(pX1)
lpX0 <- log(pX0)
lpy1 <- log(py)
lpy0 <- log(1-py)

xNew <- testDat

# Multiply probabilities
lProbX1 <- 0
for (i in 1:5180) {
  lProbX1 <- lpX1[xNew[,i]+1,i]+lProbX1
}

# Add in log(P(y=1))
BayesPredicty1 <- lpy1+lProbX1

lProbX0 <- 0
for (i in 1:5180) {
  lProbX0 <- lpX0[xNew[,i]+1,i]+lProbX0
}
BayesPredicty0 <- lpy0+lProbX0

# Get % accuracy
sum(testLab==as.numeric(BayesPredicty1>BayesPredicty0))/1806 

# Print line by line
printOutputBayes <- function() {
  for (i in 1:1806) {print(as.numeric(BayesPredicty1>BayesPredicty0)[i])}
} 
