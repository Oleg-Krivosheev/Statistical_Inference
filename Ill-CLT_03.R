library(ggplot2)

set.seed(12345)

# parameters of the distributions
n <- 100

# number of simulation
nosim <- 10000

# expected uniform mean/variance
mu <- 0.5
s2 <- 1.0/12.0

# sampling
t <- apply(matrix(runif(nosim * n), nosim), 1, mean)

#
g <- ggplot()
g <- g + aes(t)+ geom_histogram(binwidth=0.01, aes(y = ..density..), colour="black", fill="salmon")
g <- g + geom_density(colour="red", size=1)
g <- g + stat_function(fun = function(x) dnorm(x, mean = mu, sd = sqrt(s2/n)), colour="blue", size=1)
g <- g + labs(title = paste('Nsim = ', nosim, ', N = ', n))
print(g)

print(paste('Sampled    mean = ', mean(t), ', Sampled SD = ', sd(t)))
print(paste('Population mean = ', 0.0,     ', Population SD = ', sqrt(s2/n)))
