library(ggplot2)

set.seed(22345)

## parameters of the distributions
n <- 100
p <- 0.5

## number of simulation
nosim <- 10000

## expected binomial mean/variance
mu <- p
s2 <- p * (1.0 - p)

## sampling
r <- matrix(rbinom(nosim * n, size=1, prob = p), nosim)

u <- apply(r, 1, sum)
m <- apply(r, 1, mean)
s <- apply(r, 1, sd)

## plotting
g <- ggplot()
g <- g + aes(m)+ geom_histogram(binwidth=0.01, aes(y = ..density..), colour="black", fill="blue")
g <- g + labs(title = paste('Nsim = ', nosim, ', N = ', n))
g <- g + stat_function(fun = function(x) dnorm(x, mean = mu, sd = sqrt(s2/n)), colour="salmon", size=1)
#g <- g + geom_vline(xintercept = mu + 1.96*sqrt(s2/n), colour="pink", size=1)
#g <- g + geom_vline(xintercept = mu - 1.96*sqrt(s2/n), colour="pink", size=1)
g <- g + geom_vline(xintercept = mean(m) + 1.96*sd(m), colour="pink", size=1)
g <- g + geom_vline(xintercept = mean(m) - 1.96*sd(m), colour="pink", size=1)
g <- g + geom_vline(xintercept = 0.34, colour="red", size=1)
print(g)

## p-value of gaussian
2*pnorm(0.34, mean = mean(m), sd=sd(m))
