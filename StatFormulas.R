# Example for Statistical Measures 
####### Var, Covar and Corr #######
#sessioninfo()
library(lattice)
library(caret)
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(stats)
#
set.seed(123)
x <- rnorm(n=1000)
#head(x)
y <- rnorm(n=1000)
#head(y)
#x[seq(300,length(x),by=100)] <- NA

#stopifnot(anyMissing(x) == any(is.na(x)))
#anyMissing(x)

z <- cbind (x,y)
head(z)
var(x, y, na.rm = FALSE, use = "na.or.complete")
#var(x, y, na.rm = TRUE)
cov(x, y, use = "na.or.complete", method = c("pearson", "kendall", "spearman"))
#cor(x, y, use = "na.or.complete", method = c("pearson", "kendall", "spearman"))
sd(x, na.rm = TRUE)
mean(x, na.rm = TRUE)
median(x, na.rm = TRUE)
range(x, na.rm=T)
summary(z)

args(cor)
#demo(cor)
example(cor)

###### Regression #######
library(datasets)
data(airquality)
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
fit

pred <- predict(fit, newdata = airquality)
confusionMatrix(pred, airquality$Ozone)

# Latex and Html output
#library(xtable)
#xt<- xtable(summary(fit))
#xt
#print(xt, type="html") ## latex or html for Rmd documents

################## Density #################
## Probability Density Function or(PDF), or Probability Density of the variable
data(mtcars)
histogram(mtcars$mpg)
mpgdens <- density(mtcars$mpg)

## This produces Density Plot
plot(mpgdens)
# This produces Histogram with Density Overlay
hist(mtcars$mpg, col='grey', freq=FALSE)
lines(mpgdens)

############## Data Correlation ################
names(iris)     
plot(iris[-5])
with(iris, cor(Petal.Width, Petal.Length))

iris.cor <- cor(iris[-5])
str(iris.cor)
head(iris.cor)
iris.cor['Petal.Width', 'Petal.Length']

# Creating a normal disrtribution with mean = 1 and std = 1
set.seed(2)
x <- rnorm(100, mean = 1, sd = 1 )
summary(x)
mean(x)
sd(x)

##### Variance ############
nosim <- 1000
n <- 10
apply(matrix(rnorm(nosim * n), nosim), 1, mean)
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
1 / sqrt(n)

nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1 / sqrt(12 * n)

nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
2 / sqrt(n)

nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE), nosim), 1, mean))
1 / (2 * sqrt(n))

library(UsingR); data(father.son); 
x <- father.son$sheight
n<-length(x)
hist(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)

############## Common Distributions ###############
pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)

pnorm(2.8, lower.tail = FALSE)

pbinom(2, size = 500, prob = 0.01)

ppois(2, lambda = 500 * 0.01)

pbinom(4, size = 5, prob = 0.5)

############### Asymptotics ##########
n <- 10000
means <- cumsum(rnorm(n))/(1:n)
library(ggplot2)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12
(mean(x) + c(-2, 2) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12
(mean(x) + c(-3, 3) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12

round(1/sqrt(10^(1:6)), 3)
0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
binom.test(56, 100)$conf.int

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
plot(pvals, coverage, type = "l")

n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
plot(pvals, coverage2, type = "l")

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})
plot(pvals, coverage, type = "l")

############## Poisson interval
x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-3, 3) * qnorm(0.975) * sqrt(lambda/t), 3)

poisson.test(x, T = 94.32)$conf

lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda) {
        lhats <- rpois(nosim, lambda = lambda * t)/t
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})

plot(lambdavals, coverage, type = "l")

################ Linear Regression ####################
# linear model in R, where you use a factor as a predictor variable to model a response variable.Of course, predictor variables also can be continuous variables. For example, the weight of a car obviously has an influence on the mileage. But it would be nice to have an idea about the magnitude of that influence. Essentially, you want to find the equation that represents the trend line. You find the data you need for checking this in the dataset mtcars.To model the mileage in function of the weight of a car, you use the lm() function, like this:        
Model <- lm(mpg ~ wt, data=mtcars)

coef.Model <- coef(Model)
coef.Model

plot(mpg ~ wt, data = mtcars)
abline(a=coef.Model[1], b=coef.Model[2])

# Function    What It Does
# coef()      Returns a vector with the coefficients from the model
# confint()   Returns a matrix with the upper and lower limit of the confidence interval for each coefficient of the model
# fitted()    Returns a vector with the fitted values for every observation
# residuals() Returns a vector with the residuals for every observation
# vcov()      Returns the variance-covariance matrix for the coefficient
