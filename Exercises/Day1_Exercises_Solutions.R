#R code from vignette source 'Day1_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day1_Exercises_Solutions.Rnw:66-69 (eval = FALSE)
###################################################
data(chickwts)
head(chickwts)
# ?chickwts


###################################################
#code chunk number 2: Day1_Exercises_Solutions.Rnw:73-85 (eval = FALSE)
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
boxplot(weight ~ feed, data = chickwts, col = "lightgray",
        varwidth = TRUE, notch = TRUE, main = "chickwt data",
        ylab = "Weight at six weeks (gm)")


###################################################
#code chunk number 3: Day1_Exercises_Solutions.Rnw:95-101 (eval = FALSE)
###################################################
a <- c(1, 2, 3, 4)
b <- c("d", "h", "h", "d")
c <- factor(c("male", "female", "male", "female"),
            levels = c("female", "male"))
dat <- data.frame(a, b, c)
dat


###################################################
#code chunk number 4: Day1_Exercises_Solutions.Rnw:108-110 (eval = FALSE)
###################################################
# install.packages("MASS")
library("MASS")


###################################################
#code chunk number 5: Day1_Exercises_Solutions.Rnw:114-117 (eval = FALSE)
###################################################
data(bacteria)
head(bacteria)
# ?bacteria


###################################################
#code chunk number 6: Day1_Exercises_Solutions.Rnw:121-141 (eval = FALSE)
###################################################
summary(bacteria)
table(bacteria$week)
barplot(table(bacteria$week))
barplot(table(bacteria$trt))
table(bacteria$trt, bacteria$ap)
table(bacteria$trt, bacteria$y)

fisher.test(table(bacteria$trt, bacteria$y))

prop.table(table(bacteria$trt, bacteria$y))
prop.table(table(bacteria$trt, bacteria$y), margin = 1)
prop.table(table(bacteria$trt, bacteria$y), margin = 2)

plot(prop.table(table(bacteria$trt, bacteria$y)))
mosaicplot(~trt + y, data = bacteria)
barplot(prop.table(table(bacteria$y, bacteria$trt),margin=1), beside=TRUE)
barplot(prop.table(table(bacteria$trt, bacteria$y),margin=1), beside=TRUE)
barplot(prop.table(table(bacteria$y, bacteria$trt),margin=1), beside=FALSE)
barplot(prop.table(table(bacteria$trt, bacteria$y),margin=1), beside=FALSE)
?barplot


###################################################
#code chunk number 7: Day1_Exercises_Solutions.Rnw:145-151 (eval = FALSE)
###################################################
subset(bacteria, week == 2)
ss <- subset(bacteria, week == 2)
summary(ss)
# Check if we only have observations of week 2.
table(bacteria$week)
table(ss$week)


