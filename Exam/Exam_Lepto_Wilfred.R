# Exercise 1

#a) Open R-Script and read in the lepto_exam.csv data file.
data(lepto_exam)
head(lepto_exam, 10)
lepto <- data.frame(lepto_exam) #(b) Save the lepto_exam data frame as lepto data frame.
str(lepto)  #(c) Have a look at the str(...), summary(...), head(...).
summary(lepto)
head(lepto)

#(d) How many observations are in the lepto data frame? Answer=200 observations with 6 variables

#(e)What data type does each variable have? Is this appropriate? Answer=variable x is number of observations and an integer, indicating counts of individual rats in the experiment. it is appropriate

boxplot(age~gender, data = lepto, notch=FALSE, col="orange", main="Age vs Gender", xlab="gender", ylab="age")
# (f) Plot in one graph, two boxplots showing the distribution of age for both gender.

table(lepto$exposure,lepto$antibodies)#(g) Make a 2-by-2 table for exposure and antibodies.

fisher.test(table(lepto$exposure,lepto$antibodies))
#Answer to (h). exposure is a strong risk factor with Odds ratio 95% confidence interval of 0.03400112 0.15025665. Gender is not, the 95% CI  0.5828733 1.9159413
table(lepto$gender,lepto$antibodies)

fisher.test(table(lepto$gender,lepto$antibodies))

# Exercise 2
set.seed(2018) # Answer to (a)
n <- 100
rnorm(n, mean = 32.5, sd = 6.4)
Goatwts<-rnorm(n, mean = 32.5, sd = 6.4)
qqnorm(Goatwts) # Answer to (b)
qqline(Goatwts)
?replace

replace(Goatwts, 1:10, 200)#Answer to (c)
Goatwts.new<-replace(Goatwts, 1:10, 200)

qqnorm(Goatwts.new)# Answer to (d)
qqline(Goatwts.new)


