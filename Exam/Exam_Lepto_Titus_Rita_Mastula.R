rm(list=ls())
#Rscript lepto_exam.csv
lepto_exam <- read.csv("~/RCourse/Exam/lepto_exam.csv")
View(lepto_exam)

#Save the lepto_exam data frame as lepto data frame
lepto<-data.frame(lepto_exam)

# A look at the str(...),summary(...),head(...). 
str(lepto)
summary(lepto)
head(lepto)

#observations in the lepto data frame? 

#What data type does each variable have? Is this appropriate?
typeof(lepto$X)
typeof(lepto$gender)
typeof(lepto$age)
typeof(lepto$exposure)
typeof(lepto$antibodies)


#Plot in one graph,two box plots showing the distribution of age for both gender 
boxplot(lepto$age ~ lepto$gender, data = lepto)

#Make a 2-by-2 table for exposure and antibodies
table(lepto$exposure, lepto$antibodies)

#Quantify the e???ect of the two potential risk factors exposure and gender with an odds ratio (OR).
#What is the 95%-con???dence interval? 
#Interpret the odds ratio as well as the 95%-con???denceinterval

chisq.test(table(lepto$exposure,lepto$antibodies))
fisher.test(table(lepto$exposure,lepto$gender))

#Clearing the enviroment
rm(list = ls())           
#Simulate a continuous vector with length n = 100 representing the body weight of adult goats.
#Before simulating, set the seed by set.seed(2018). 
#(b)Assess if the above simulated variable is normally distributed. 
#(c)Replace the ???rst 10 observations of your previously simulated vector with the values 200. 
#(d)Check again for normality. 
set.seed(2018)
n  = 100
x = runif(n, min=1, max=100)
x
shapiro.test(x)
plot(x)
qqnorm(x)
qqline(x, col="red")

x[1:10]
x[1:10]=200
x
shapiro.test(x)
plot(x)
qqnorm(x)
qqline(x, col="red")
