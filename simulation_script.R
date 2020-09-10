library(ggplot2)

N <- 40 # number of averages
lambda <- 0.2 # rate parameter
nSims <- 1000

set.seed(5730)
simData <- replicate(nSims, rexp(n = N, rate = lambda))

means <- apply(simData, MARGIN = 2, FUN = mean)
sds <- apply(simData, MARGIN = 2, FUN = sd)
vars <- apply(simData, MARGIN = 2, FUN = var)

truMean <- 1/lambda
truSD <- 1/lambda
truVar <- truSD^2


meanNorm <- rnorm(1000, mean = 5, sd = 5)


ggplot(data = as.data.frame(meanNorm)) +
    geom_histogram(aes(x=meanNorm))

meanDist <- ggplot(data = as.data.frame(means)) +
    geom_density(aes(x = means)) +
    geom_vline(xintercept = truMean, color = "red")
meanDist

sdDist <- ggplot(data = as.data.frame(sds)) +
    geom_density(aes(x = sds)) +
    geom_vline(xintercept = truSD, color = "blue")
sdDist

varDist <- ggplot(data = as.data.frame(vars)) +
    geom_density(aes(x = vars)) +
    geom_vline(xintercept = truVar, color = "green")
varDist




#### Tooth Growth

data("ToothGrowth")

str(ToothGrowth)
ggplot(ToothGrowth) +
    geom_boxplot(aes(x = supp, y = len, fill = supp))

ggplot(ToothGrowth) +
    geom_boxplot(aes(x = as.factor(dose), y = len, fill = as.factor(supp))) +
    ggtitle( "Tooth Length by Dosage and Supplement Type") +
    xlab("Dosage") +
    ylab("Odontoblast Length") 



lapply(ToothGrowth, INDEX = ToothGrowth$supp, FUN = mean)

suppByDose <- split(ToothGrowth, list(ToothGrowth$supp, as.factor(ToothGrowth$dose)))

sapply(suppByDose, function(x) mean(x[,1]))


