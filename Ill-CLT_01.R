library(ggplot2)

set.seed(12345)

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

m <- apply(r, 1, mean)
s <- apply(r, 1, sd)

#
g <- ggplot()
g <- g + aes(m)+ geom_histogram(binwidth=0.01, aes(y = ..density..), colour="black", fill="salmon")
g <- g + geom_density(colour="red", size=1)
g <- g + stat_function(fun = function(x) dnorm(x, mean = mu, sd = sqrt(s2/n)), colour="blue", size=1)
g <- g + labs(title = paste('Nsim = ', nosim, ', N = ', n))
print(g)

print(paste('Sampled    mean = ', mean(m), ', Sampled SD = ', sd(m)))
print(paste('Population mean = ', mu,     ', Population SD = ', sqrt(s2/n)))

#r.min <- NULL
#r.max <- NULL
#for(i in 1:nosim) {
#    r.min[i] <- m[i] - 1.96*s[i]/sqrt(n)
#    r.max[i] <- m[i] + 1.96*s[i]/sqrt(n)
#}

#j <- 0
#for (i in 1:nosim) {
#    if (r.min[i] < p*n - 2.0*sqrt(s2/n) & r.max[i] > p*n + 2.0*sqrt(s2/n)) {
#        j <- j + 1
#    }
#}

#coverage <- j/nosim
#coverage
