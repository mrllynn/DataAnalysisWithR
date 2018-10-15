set.seed(3105) # for repeatability set the seed

# simulate a data set with three missing values ##

a <- rep(1:3,10) # create a vector with ten times 
# repeating the numbers 1,2, 3
a


b <- a*rnorm(30,100,10) # crete a vector with 30 random
# numbers drawn from a normal distribution with a mean =100, and a sd=10
b
b.7.orginal <- b[7] # to save the original value
b[7] <- NA # to advertantly put the seventh value in b to NA

b.8.original <- b[8]
b[8] <- NA

b.9.original <- b[9]
b[9] <- NA

c <- a*rpois(30,12) # generate a vector with 30 random numbers,
# drawn from a Poisson distribution with lambda = 12

mydat <- data.frame(a,b,c) # create a dataframe
str(mydat)

table(complete.cases(mydat))
apply(mydat,2,function(x) sum(is.na(x))) # check in which varianles there are missing values
dim(mydat)

#impute missing values
library(missForest)



dat.imp <- missForest(mydat, verbose = TRUE, variablewise = TRUE) 
dat.imp$OOBrerror

mydat.i <- dat.imp$ximp

apply(mydat.i,2,function(x) sum(is.na(x))) # check again that in the 
# imputed data set calle "mydat.i" there are no missing values
dim(mydat.i)

mydat.i[7,2] # to compare the original value which was put to NA
# with the imputed value
b.7.orginal

mydat.i[8,2]
b.8.orginal

mydat.i[9,2]
b.9.orginal
