# MCMC Metropolis algorithm using a normal proposal distribution 
# with various standard deviations.
# Assumes for generality an unnormalized univariate density "posterior(x)"

# This code demands a density that is defined everywhere that might 
# be proposed. Use a density with infinite support or use an 
# "if (!(x in domain)) {dens(x) = 0}" allowance in your posterior 
# function's definition.

# TODO: Add adaptive phase for the proposal distribution to ensure a reasonable acceptance rate

# Example posterior:
posterior <- function(x) {
  dnorm(x, mean = 0, sd = 1)        # Replace this line with desired density
}

# Working code:
proposalDelta <- function(nChains, sd) {
  return(rnorm(nChains, mean = 0, sd))
}

metSample <- function(steps, nChains = 3, init, burnin = 100) {
  SD <- runif(n = nChains, 0, 0.5)  # Generate SD values to be used in iteration. Alternatively,
                                    # uncomment the following deterministic code if efficiency 
                                    # isn't good:
                                    # SD <- seq(from = 0, to = 1, length.out = nChains)
  MCMCarray <- array(dim=c(steps+burnin, nChains))            # Generate array with nChains columns
  if (length(init) == 1) {init <- rep(init, times = nChains)} # Use same initial value for all chains if desired
  if (length(init) != nChains) {stop("Please use a single starting value, or a vector with length(init)=nChains") }
  MCMCarray[1,] <- init

  for (i in 1:(steps+burnin-1)) {
      currentState <- MCMCarray[i,]
      proposalState <- currentState + proposalDelta(nChains, sd=SD)
      ran <- runif(n = 1, 0, 1)
      ratio <- posterior(proposalState)/posterior(currentState)
      accept <- ratio > ran
      for (j in 1:nChains) {
        
          if (accept[j]) { MCMCarray[i+1, j] <- proposalState[j] }    # Send proposal to next row
          else { MCMCarray[i+1,j] <- currentState[j] }                # Repeat current state, send to next row
      }
  }
  print(c("The standard deviations used for the proposal distribution:", SD), quote = F)
  return(MCMCarray[-(1:burnin),])
}


