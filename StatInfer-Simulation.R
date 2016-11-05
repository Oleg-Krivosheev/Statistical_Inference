library(ggplot2)
library(datasets)

## Example for Statistical Inference:
lambda = 0.2
n = 100 # The number of samples per simulation
nosim = 1000 # The number of simulations
st <- replicate(nosim,rexp(n,lambda)); p <- NULL
for (i in 1:nosim){p[i] <- mean(st[,i])}
p <- as.data.frame(p); names(p)<-"mean"

## Show how variable it is and compare it to the theoretical variance of the distribution.
est.sd <- sd(p$mean); est.v <- est.sd ^ 2 / nosim; the.sd <- (1/lambda * 1/sqrt(n)); the.v <- the.sd ^ 2 / nosim
p2 <- matrix(c(est.sd, est.v, the.sd, the.v), nrow = 2, ncol = 2)
rownames(p2) <- c("St. Dev. of Sample Mean","Variance of Sample Mean")
colnames(p2) <- c("Estimated","Theoretical")
p2

## Show that the distribution is approximately normal
#data(aggdata_health_top10_fatal)
#ggplot(data=p,aes(y=INJURIES,x=EVTYPE))+geom_bar(size=1,colour="black",fill="red",stat="identity")+labs(list(x="Event",y="Injuries"))
#g <- ggplot(aggdata_health_top10_fatal, aes(aggdata_health_top10_fatal$event_type, aggdata_health_top10_fatal$fatalities))

# Overlays a normal distribution over the density plot to show the normal approximation.
#g + stat_function(fun=dnorm, args=list(mean=5, sd=est.sd),color = "darkblue", size = 1.5)

rng.min<-NULL; rng.max<-NULL
for(i in 1:nosim){
        rng.min[i]<-mean(st[,i])-1.96*sd(st[,i])/sqrt(n)
        rng.max[i]<-mean(st[,i])+1.96*sd(st[,i])/sqrt(n)
}

p$min<-rng.min
p$max<-rng.max

j <- 0
for (i in 1:nosim) {
    if (5 > p$min[i] & 5 < p$max[i]) {
        j <- j + 1
    }
}

coverage <- j/nosim
coverage
