#set lepto as data. frame andRead structure
lepto<-data.frame(lepto_exam)
str(lepto)

##Read summary
summary(lepto)

# Show the first 6 observations
head(lepto)

#Observations that are in the lepto data frame
## 200 obs. of 6 variables

#Type of each variable and If  appropriate

class(lepto$X)
class(lepto$age)
class(lepto$gender)
class(lepto$antibodies)
class(lepto$exposure)
class(lepto$num.rats)

#boxplot and table
boxplot(age~gender, data = lepto)
table(lepto$antibodies, lepto$exposure)

#run a chisquare test and fishers test
chisq.test(table(lepto$antibodies, lepto$exposure))
fisher.test(table(lepto$antibodies, lepto$exposure))



#QN 2a Simulate a continuous vector with length n = 100 representing the body weight of adult goats.
#set.seed
getwd
set.seed(2018)
n=100
Gtwt = rnorm(100, mean=25,sd=10)

#Test for normal distribution
shapiro.test(Gtwt)


plot(Gtwt)
qqnorm(Gtwt)
qqline(Gtwt, col="green")
hist(Gtwt)


#Replace the firrst 10 observations of your previously simulated vector with the values 200
Gtwt[1:10]=200
str(Gtwt)
Gtwt
Gtwt[1:10]=200
Gtwt
str(Gtwt)

#Test for normal distribution again
shapiro.test(Gtwt)
plot(Gtwt)
qqnorm(Gtwt)
qqline(Gtwt, col="orange")
hist(Gtwt)




#othes on lepto
class(lepto) 
sapply(lepto, class(lepto))
apply(lepto, 2, class)
str(lepto)

