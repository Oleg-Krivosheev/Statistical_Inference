attach(InsectSprays)
data(InsectSprays)
str(InsectSprays)

## Let's look at the means
tapply(count, spray, mean)

## Let's look at the SD
tapply(count, spray, sd)

## Let's look at the length of the samples
tapply(count, spray, length)

boxplot(count ~ spray)

## one way test
one <- oneway.test(count~spray)
print(one)

## ANOVA using aov
aov.out = aov(count ~ spray, data=InsectSprays)
summary(aov.out)

## Tukey HSD(Honestly Significant Difference)
TukeyHSD(aov.out)
