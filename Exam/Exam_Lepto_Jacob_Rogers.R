#exercise 1
lepto_exam <- read.csv("E:/year3sem2/R course folder/Exam/lepto_exam.csv")
View(lepto_exam)

lepto <- data.frame(lepto_exam)
str(lepto)
summary(lepto)
head(lepto)

# returns number of rows (number of obs) and columns in lepto data frame
dim(lepto)

# returns data type of ech variable in lepto
class(lepto$X)
class(lepto$gender)
class(lepto$age)
class(lepto$exposure)
class(lepto$antibodies)
class(lepto$num.rats)

boxplot(lepto$age ~ lepto$gender, ylab = "age", xlab ="gender")

# returrns 2 by 2 table of exposure and antibodies
table(lepto$exposure, lepto$antibodies)
# returns proportions of people with or without lepto antibodies in rural and urban
prop.table(table(lepto$exposure, lepto$antibodies))

chisq.test(table(lepto$exposure, lepto$antibodies))
#fisher.test with exposure and antibodies
fisher.test(table(lepto$exposure, lepto$antibodies))
#fishers.test p-value (2.475e-16) is close to zero (significant) hence reject null hypothesis
#confidence inteval is betwween 0.03 to 0.15 far from 1, reject null hypothesis
#odds of being positive for lepto.antibodies in rural is 0.073 than in urban

table(lepto$gender, lepto$antibodies)
fisher.test(table(lepto$gender, lepto$antibodies))
#fishers.test p-value( 0.8875) not significant, no evidence to reject null hypothesis
#confidence inteval for OR contains 1, hence no association between gender and antibodies

#exercise 2
set.seed(2018)
n <- 100

weight <- rnorm(100, mean = 35, sd = 10)
weight
plot(weight)
plot(density(weight))

#checking for normlity
shapiro.test(weight)
#p-value = 0.3077, no enough evidence to reject null hypothesis (there is normality)
qqnorm(weight)
qqline(weight, col = "red",lwd= 2)
par(mfrow =c(1,1))


?replace
weight1 <- replace(weight, 1:10, 200)
plot(weight1)
plot(density(weight1))


#checking for normlity
shapiro.test(weight1)
#p-value = 2.2e-16, reject null hypothesis (no normality)
qqnorm(weight1)
qqline(weight1, col = "red",lwd= 2)
par(mfrow =c(1,1))

