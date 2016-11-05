library(ggplot2)

n <- 10000
means <- cumsum(rnorm(n))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0, color="blue", size=1) + geom_line()
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
print(g)

readline(prompt="Press [enter] to continue")

means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5, color="blue", size=1) + geom_line()
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
print(g)
