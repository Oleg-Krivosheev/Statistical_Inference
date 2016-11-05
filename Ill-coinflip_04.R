library(ggplot2)

g <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
g <- g + stat_function(fun = function(x) dnorm(x, mean = 0, sd = 1), colour="salmon", size=1)
g <- g + stat_function(fun = function(x) dt(x, df=7), colour="green", size=1)
g <- g + geom_vline(xintercept = 0.0 + 1.96, colour="pink", size=1)
g <- g + geom_vline(xintercept = 0.0 - 1.96, colour="pink", size=1)
g <- g + xlim(-4, 4) + ylim(0.0, 0.6)
print(g)

pnorm(-1.96, lower.tail=FALSE) - pnorm(1.96, lower.tail=FALSE)
