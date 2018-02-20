#R code from vignette source 'Day2_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day2_Exercises_Solutions.Rnw:63-65 (eval = FALSE)
###################################################
chickwts[, 2]
summary(aov(weight ~ feed, data = chickwts))


###################################################
#code chunk number 2: Day2_Exercises_Solutions.Rnw:68-75 (eval = FALSE)
###################################################
# SOLUTION: [...] the squared brackets we need to select rows and columns of a
# data frame.
# (...) the round brackets we need around function calls,
# e. g.:
subset(...) # to define a subset
c(...) # to define a vector
data.frame(...) # to define a data frame


###################################################
#code chunk number 3: Day2_Exercises_Solutions.Rnw:81-86 (eval = FALSE)
###################################################
str(bacteria)
head(bacteria$trt)
table(bacteria$trt)
levels(bacteria$trt)
nlevels(bacteria$trt)


###################################################
#code chunk number 4: Day2_Exercises_Solutions.Rnw:91-106 (eval = FALSE)
###################################################
table(bacteria$trt)
# OPTION 1:
# Test how many levels are in the variable "trt"?
levels(bacteria$trt)
bacteria$trt.new <- bacteria$trt
# Overwrite the levels "placebo", "drug", "drug+" with new
# levels called "placebo", "drug", "drug" --> combine "drug" and "drug+"
levels(bacteria$trt.new) <- c("placebo", "drug", "drug")
# Do table for variable "trt" and "trt.new" to see if you combined correctly
table(bacteria$trt)
table(bacteria$trt.new)
# Rename the levels from "placebo", "drug" to "placebo", "treated"
levels(bacteria$trt.new) <- c("placebo", "treated")
# Do another table to check if you did everything correctly:
table(bacteria$trt.new)


###################################################
#code chunk number 5: Day2_Exercises_Solutions.Rnw:109-115 (eval = FALSE)
###################################################
summary(bacteria)
table(bacteria$trt.new)
barplot(table(bacteria$trt.new))
table(bacteria$trt.new, bacteria$ap)
table(bacteria$trt.new, bacteria$y)
plot(table(bacteria$trt.new, bacteria$y))


###################################################
#code chunk number 6: Day2_Exercises_Solutions.Rnw:122-125 (eval = FALSE)
###################################################
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)


###################################################
#code chunk number 7: Day2_Exercises_Solutions.Rnw:128-143 (eval = FALSE)
###################################################
table(ToothGrowth$supp)
tapply(ToothGrowth$len, ToothGrowth$supp, mean)
tapply(ToothGrowth$len, ToothGrowth$supp, median)
tapply(ToothGrowth$len, ToothGrowth$supp, sd)

tapply(ToothGrowth$len, ToothGrowth$dose, mean)
tapply(ToothGrowth$len, ToothGrowth$dose, median)
tapply(ToothGrowth$len, ToothGrowth$dose, sd)

barplot(table(ToothGrowth$supp))
hist(ToothGrowth$len)
# install.packages("graphics")
library("graphics")
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")


###################################################
#code chunk number 8: Day2_Exercises_Solutions.Rnw:146-151 (eval = FALSE)
###################################################
table(ToothGrowth$dose)
class(ToothGrowth$dose)
ToothGrowth$dose.fac <- factor(ToothGrowth$dose, levels = c("0.5", "1", "2"))
class(ToothGrowth$dose.fac)
table(ToothGrowth$dose.fac)


###################################################
#code chunk number 9: Day2_Exercises_Solutions.Rnw:156-182 (eval = FALSE)
###################################################
# supp: VC, OJ
sub.OJ <- subset(ToothGrowth, supp == "OJ")
sub.VC <- subset(ToothGrowth, supp == "VC")
# graphically
qqnorm(sub.OJ$len)
qqline(sub.OJ$len)
qqnorm(sub.VC$len)
qqline(sub.VC$len)
# with a statistical test
shapiro.test(sub.OJ$len)
shapiro.test(sub.VC$len)
# dose: 0.5, 1, 2
sub.0.5 <- subset(ToothGrowth, dose.fac == "0.5")
sub.1 <- subset(ToothGrowth, dose.fac == "1")
sub.2 <- subset(ToothGrowth, dose.fac == "2")
# graphically
qqnorm(sub.0.5$len)
qqline(sub.0.5$len)
qqnorm(sub.1$len)
qqline(sub.1$len)
qqnorm(sub.2$len)
qqline(sub.2$len)
# with a statistical test
shapiro.test(sub.0.5$len)
shapiro.test(sub.1$len)
shapiro.test(sub.2$len)


###################################################
#code chunk number 10: Day2_Exercises_Solutions.Rnw:204-216 (eval = FALSE)
###################################################
# OPTION 1:
# install.packages("readr")
library("readr")
lung <- read_delim("~/Dropbox/201710_Makerere/03_Exercises/data/perulung_ems.csv",
                   ";", escape_double = FALSE, trim_ws = TRUE)
lung <- data.frame(lung)
# OPTION 2:
# Import .csv file with the help of the read.csv function
# Be sure to add sep = ";" so that we separate the columns.
lung <- read.csv("C:\\Users\\Exercises\\data\\perulung_ems.csv", sep = ";")
head(lung)
str(lung)


###################################################
#code chunk number 11: Day2_Exercises_Solutions.Rnw:220-228 (eval = FALSE)
###################################################
head(lung)
str(lung)
lung$sex <- factor(lung$sex, levels = c("0", "1"))
# levels(lung$sex) <- c("female", "male")
# levels(lung$sex)[levels(lung$sex)=="0"] <- "female"
# levels(lung$sex)[levels(lung$sex)=="1"] <- "male"
# tapply(lung$fev1, lung$sex, mean)
lung$respsymptoms <- factor(lung$respsymptoms, levels = c("0", "1"))


###################################################
#code chunk number 12: Day2_Exercises_Solutions.Rnw:232-240 (eval = FALSE)
###################################################
library(usdm)
# check for multicollinearity by using variance inflation factors
# cerate a dataframe just with the three continuous/numeric variables fevs, age and height
try.vif <- lung[,c("fev1","height","age")];
# perform scatterplots for these three variables
pairs(try.vif)
# get the three VIF, as a rule of thumb they should be < 3
vif(try.vif)


###################################################
#code chunk number 13: Day2_Exercises_Solutions.Rnw:243-247 (eval = FALSE)
###################################################
# Check for heteroscedascity or homogeneity of variances
?bartlett.test
data("chickwts")
bartlett.test(weight ~ feed, data = chickwts)


