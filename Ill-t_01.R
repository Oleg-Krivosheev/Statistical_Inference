library(manipulate)

pvals <- seq(.5, .99, by = .01)
myplot2 <- function(df){
    d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
                    p = pvals)
    g <- ggplot(d, aes(x= n, y = t))
    g <- g + geom_abline(size = 2, col = "lightblue")
    g <- g + geom_line(size = 2, col = "black")
    g <- g + geom_vline(xintercept = qnorm(0.975))
    g <- g + geom_hline(yintercept = qt(0.975, df))
    g
}

manipulate(myplot2(df), df = slider(1, 20, step = 1))
