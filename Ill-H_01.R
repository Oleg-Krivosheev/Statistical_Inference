father.son <- read.csv(file="F.S.csv")

summary(father.son)

p <- ggplot(father.son, aes(fheight, sheight))
p + geom_point()

q <- t.test(father.son$sheight - father.son$fheight)
p <- t.test(father.son$sheight, father.son$fheight, paired = TRUE)

print(q)
print(p)
