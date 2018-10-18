library(MASS)
lepto <- data.frame(lepto_exam)
str(lepto)
summary(lepto)
head(lepto)
# 200 observations
# x=continuous
#gender=categorical
#age=continuous
#exposure=categorical
#antibodies=categorical
#num.rats=continuous
boxplot(lepto$age,lepto$gender)
table(lepto$antibodies,lepto$exposure)
chisq.test(table(lepto$exposure,lepto$antibodies))
fisher.test(table(lepto$exposure,lepto$gender))
#odds ratio =1.270562 
#CI= 0.7029327 2.3033895
#p-value = 0.4792
#The development of antibodies by gender increases by 1.27 times for every unit increase in exposure


#exercise 2
set.seed(2018)
n<-100
rnorm(n,mean = 15,sd=1)
qqnorm(rnorm(n,mean = 15,sd=1))
qqline(rnorm(n,mean = 15,sd=1))
x<-rnorm(n,mean = 15,sd=1)
replace(x,1:10,200)
qqnorm(replace(x,1:10,200))
qqline(replace(x,1:10,200))

