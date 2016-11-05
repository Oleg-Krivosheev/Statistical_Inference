library(ggplot2)
library(data.table)
library(R.utils)

library(datasets)
data(ToothGrowth)

tg <- as.data.table(ToothGrowth) # data loaded as data.table

print(str(tg))

print(summary(tg))

print(table(tg$supp, tg$dose))

# We plot length versus dose, for a different supplement marked by color.
g <- ggplot(tg, aes(x = dose, y = len, colour = supp))
g <- g + geom_point() + xlab("Dose, mg/day") + ylab("Length, mm")
# g <- g + ggtitle("Length vs Dose")
print(g)

# Next, we explore mean teeth length values grouped by supplement and dose, as well as std.deviation.
q <- tg[, .(mean(len), sd(len)), by=c("dose", "supp")]
setnames(q, "V1", "mean_len")
setnames(q, "V2", "std.dev")
print(q)

#Our null hypothesis is that the mean tooth growth for
#the orange juice (OJ) group is equal to the mean tooth growth of the
#ascorbic acid (VC) group, for the same dosage of vitamin C.

# $H_0: \mu_{OJ} - \mu_{VC} == 0$

# The alternative hypothesis is that they are not equal.

# $H_a: \mu_{OJ} - \mu_{VC} > 0$

# We assume $\alpha=0.05$, which corresponds to a 95% confidence interval.

# We split our data table into three ones by the dose value.

split.data.table <- function(x, f, drop = FALSE, by, flatten = FALSE, ...){
    if(missing(by) && !missing(f)) by = f
    stopifnot(!missing(by), is.character(by), is.logical(drop), is.logical(flatten), !".ll" %in% names(x), by %in% names(x), !"nm" %in% by)
    if(!flatten){
        .by = by[1L]
        tmp = x[, list(.ll=list(.SD)), by = .by, .SDcols = if(drop) setdiff(names(x), .by) else names(x)]
        setattr(ll <- tmp$.ll, "names", tmp[[.by]])
        if(length(by) > 1L) return(lapply(ll, split.data.table, drop = drop, by = by[-1L])) else return(ll)
    } else {
        tmp = x[, list(.ll=list(.SD)), by=by, .SDcols = if(drop) setdiff(names(x), by) else names(x)]
        setattr(ll <- tmp$.ll, 'names', tmp[, .(nm = paste(.SD, collapse = ".")), by = by, .SDcols = by]$nm)
        return(ll)
    }
}

t <- split.data.table(tg, by = "dose", drop= TRUE, flatten = TRUE)

### Dose Value 0.5 mg/day
t_05 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = t[[1]])
p_05 <- round(t_05$p.value, 3)
printf("p-value = %.3f", p_05)
printf("95 percent confidence interval: %.3f %.3f", t_05$conf[1], t_05$conf[2])

# P-value is very small, only about `r p_05` which allow us safely reject null hypothesis
# using this most permissive test. Confidence interval is clearly above zero,
# which favors alternative hypothesis.

### Dose Value 1.0 mg/day
t_10 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = t[[2]])
p_10 <- round(t_10$p.value, 3)
printf("p-value = %.3f", p_10)
printf("95 percent confidence interval: %.3f %.3f", t_10$conf[1], t_10$conf[2])

#P-value is very small as well, about `r p_10`, which allow us reject null hypothesis.
#Confidence interval is clearly above zero, which favors alternative hypothesis.

### Dose Value 2.0 mg/day
t_20 <- t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = t[[3]])
p_20 <- round(t_20$p.value, 3)
printf("p-value = %.3f", p_20)
printf("95 percent confidence interval: %.3f %.3f", t_20$conf[1], t_20$conf[2])

# Here p-value is very large, equal to `r p_20`, confidence interval is large as well,
# almost symmetric and clearly contains 0. Here we fail to reject null hypothesis.

# Lets try for dose 2.0 mg/day t-test assuming that data are paired and variance is equal.
# Sort data first and then apply t-test with paired equal to true, and var.equal set to true as well.
# That procedure will produce smallest p-value and smallest confidence interval, which might allow us
# to reject null hypothesis.

d <- t[[3]]
d <- d[with(d, order(supp, len)), ]
t_20p <- t.test(len ~ supp, paired = TRUE, var.equal = TRUE, data = d)
p_20p <- round(t_20p$p.value, 3)
printf("p-value = %.3f", p_20p)
printf("95 percent confidence interval: %.3f %.3f", t_20p$conf[1], t_20p$conf[2])

#As one can see, confidence interval gets a lot smaller than in previous test, and p-value of `r p_20p`
#is smaller as well.  Nveretheless, it is still very clear, that we cannot reject null hypothesis.

#We observe that guinea pig teeth could grow up to 33.9mm. That would be one charming and dangerous pig!
