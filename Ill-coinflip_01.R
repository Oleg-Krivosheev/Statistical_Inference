library(ggplot2)

set.seed(22345)

# parameters of the distributions
n <- 100
p <- 0.5

# number of simulation
nosim <- 10000

# expected binomial mean/variance
mu <- p
s2 <- p * (1.0 - p)

# sampling
r <- matrix(rbinom(nosim * n, size=1, prob = p), nosim)

u <- apply(r, 1, sum)
m <- apply(r, 1, mean)
s <- apply(r, 1, sd)

#
g <- ggplot()
g <- g + aes(m)+ geom_histogram(binwidth=0.01, aes(y = ..density..), colour="black", fill="blue")
g <- g + labs(title = paste('Nsim = ', nosim, ', N = ', n))
g <- g + geom_vline(xintercept = 0.34, colour="red", size=1)
print(g)

print(paste('Sampled    mean = ', mean(m), ', Sampled SD = ', sd(m)))
print(paste('Population mean = ', mu,     ', Population SD = ', sqrt(s2/n)))
