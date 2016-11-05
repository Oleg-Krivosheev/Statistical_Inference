library(ggplot2)

set.seed(32345)

## parameters of the distributions
n <- 8
p <- 0.5

## number of simulation
nosim <- 5000

## sampling
r <- matrix(rbinom(nosim * n, size=1, prob = p), nosim)

u <- apply(r, 1, sum)
m <- apply(r, 1, mean)
s <- apply(r, 1, sd)

q <- (m - mean(m))/sd(m)

## plotting
g <- ggplot()
g <- g + aes(q)+ geom_histogram(binwidth=0.75, aes(y = ..density..), colour="black", fill="blue")
g <- g + stat_function(fun = function(x) dnorm(x, mean = 0.0, sd = 1.0), colour="salmon", size=1)
g <- g + stat_function(fun = function(x) dt(x, df=n-1), colour="green", size=1)
print(g)
