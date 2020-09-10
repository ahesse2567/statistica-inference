N <- 40 # number of averages
lambda <- 0.2 # rate parameter
nSims <- 1000

set.seed(5730)

simData <- replicate(nSims, rexp(n = N, rate = lambda))
means <- apply(simData, MARGIN = 2, FUN = mean)
