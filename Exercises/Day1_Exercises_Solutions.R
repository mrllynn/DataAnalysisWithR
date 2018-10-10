#R code from vignette source 'Exercises/Day1_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day1_Exercises_Solutions.Rnw:66-69 (eval = FALSE)
###################################################
data(chickwts)
head(chickwts)
# ?chickwts


###################################################
#code chunk number 2: Day1_Exercises_Solutions.Rnw:73-88 (eval = FALSE)
###################################################
summary(chickwts)
tapply(chickwts$weight, chickwts$feed, mean)
tapply(chickwts$weight, chickwts$feed, median)
tapply(chickwts$weight, chickwts$feed, sd)
table(chickwts$feed)
barplot(table(chickwts$feed))
boxplot(chickwts$weight ~ chickwts$feed)
# boxplot(weight ~ feed, data = chickwts)
hist(chickwts$weight)
hist(chickwts$weight, freq = FALSE)
lines(density(chickwts$weight), col = "red", lwd = 3)
boxplot(weight ~ feed, data = chickwts, col = "lightgray",
        varwidth = TRUE, main = "chickwt data",
        ylab = "Weight at six weeks (gm)")
barplot(table(chickwts$feed))


###################################################
#code chunk number 3: Day1_Exercises_Solutions.Rnw:92-99 (eval = FALSE)
###################################################
lm.mod <- lm(weight ~ feed, data = chickwts)
summary(lm.mod)
anova <- aov(weight ~ feed, data = chickwts)
TukeyHSD(anova)
summary(anova)
par(mfrow=c(2,2))
plot(lm.mod)


