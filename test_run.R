# Associated with "Naive Bayes.R", which implements a Naive Bayes algorithm on a 
# bag of words model with 5180 features and a training dataset of 1806 observations.
# test/train_labels are the classifications corresponding to each entry. Data located:
# http://rci.rutgers.edu/~pa336/HW1_data.zip

# Use setwd() to set to the directory containing the data files and "Naive Bayes.R".
# E.g.: setwd("/Users/macuser/Desktop/ML Code millerjs/")

# OPTION 1==============================
trainDat <- read.table(file = "train.csv", sep = ",")
trainLab <- read.table(file = "train_labels.txt")
testDat <- read.table(file = "test.csv", sep = ",")
testLab <- read.table(file = "test_labels.txt")
source(file = "Naive Bayes.R") # Takes about 50 seconds on my computer
# OPTION 1==============================

# OPTION 2==============================
# Run load() on the .RData file and load the environment directly
load("NaiveBayesEnviron.RData")
# OPTION 2==============================

# PRINT OUTPUT==============================
printOutputMLE() # Prints output line by line
printOutputBayes() # Prints output line by line
# PRINT OUTPUT==============================
