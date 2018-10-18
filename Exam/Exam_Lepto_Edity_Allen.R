rm(list=ls())
setwd("~/RCourse")
library(readr)

Qn 1.
lepto_exam <- read.csv("Exam/lepto_exam.csv", header=T,
                       ",")
lepto <- data.frame(lepto_exam)
str(lepto)
summary(lepto)
head(lepto,100)
boxplot(lepto$age ~ lepto$gender)
table(lepto$exposure,lepto$antibodies)
table(lepto$gender,lepto$antibodies)
table(lepto$exposure,lepto$antibodies)
chisq.test(table(lepto$exposure,lepto$antibodies))
fisher.test(table(lepto$exposure,lepto$antibodies))
# since OR=0.07320699 exposure, there is a protective effect and 95% CI, 0.03400112 - 0.15025665, it is significant with a negative effect because it does not cross the null.

table(lepto$gender,lepto$antibodies)
chisq.test(table(lepto$gender,lepto$antibodies))
fisher.test(table(lepto$gender,lepto$antibodies))
# since OR=1.056161  for gender, it is approximately 1, therefore there is no effect and 95% CI, 0.5828733 - 1.9159413 crosses the null hence statistically not significant.


Qn 2.
set.seed(2018)
n <- 100
wgt <- rnorm(n, mean=25, sd = 3)
str(wgt)
summary(wgt)
shapiro.test(wgt)
qqnorm(wgt)
qqline(wgt, col= "red")
wgt[1:10]<- 200
wgt
shapiro.test(wgt)
qqnorm(wgt)
