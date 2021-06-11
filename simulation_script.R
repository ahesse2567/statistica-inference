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


meanDist <- ggplot(data = as.data.frame(means)) +
    geom_histogram(aes(x = means)) +
    geom_vline(xintercept = truMean, color = "red")
meanDist

x <- seq(min(means), max(means), length = nSims)
hist(means, probability = T)
lines(x, dnorm(x, truMean, sd = truSD/sqrt(N)), type = "l")

#### Tooth Growth

data("ToothGrowth")
str(ToothGrowth)

help(ToothGrowth)

ggplot(ToothGrowth) +
    geom_boxplot(aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
    ggtitle( "Tooth Length by Dosage") +
    xlab("Dosage") +
    ylab("Odontoblast Length") +
    labs(fill = "Doseage")

ggplot(ToothGrowth) +
    geom_boxplot(aes(x = supp, y = len, fill = supp)) +
    ggtitle( "Tooth Length by Supplement Type") +
    xlab("Supplement Type") +
    ylab("Odontoblast Length") +
    labs(fill = "Supplement\n Type")

ggplot(ToothGrowth) +
    geom_boxplot(aes(x = as.factor(dose), y = len, fill = as.factor(supp))) +
    ggtitle( "Tooth Length by Dosage and Supplement Type") +
    xlab("Dosage") +
    ylab("Odontoblast Length") +
    labs(fill = "Supplement\nType")

suppByDose <- split(ToothGrowth, list(ToothGrowth$supp, as.factor(ToothGrowth$dose)))
sapply(suppByDose, function(x) mean(x[,1]))
sapply(suppByDose, function(x) length(x[,1]))
groupVars <- sapply(suppByDose, function(x) var(x[,1]))
max(groupVars)/min(groupVars)

library(car)
leveneTest(ToothGrowth$len ~ as.factor(ToothGrowth$dose)*ToothGrowth$supp,
           center = median)
leveneTest(ToothGrowth$len ~ as.factor(ToothGrowth$dose),
           center = median)

byDose <- split(ToothGrowth, ToothGrowth$dose)
byDose
sapply(byDose, function(x) var(x[,1]))
sapply(byDose, function(x) length(x[,1]))
t.test(byDose[2][[1]][1], byDose[3][[1]][1],
       alternative = "two.sided", paired = FALSE, var.equal = TRUE)


leveneTest(ToothGrowth$len ~ as.factor(ToothGrowth$supp),
           center = median)

bySupp <- split(ToothGrowth, ToothGrowth$supp)
bySupp
sapply(bySupp, function(x) var(x[,1]))
t.test(bySupp[1][[1]][1], bySupp[2][[1]][1],
       alternative = "greater", paired = FALSE, var.equal = TRUE)


t.test(len ~ supp, data = ToothGrowth, var.equal = TRUE, paired = FALSE)

