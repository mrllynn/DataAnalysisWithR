### R code from 'Brucellosis.sim.Rnw'

n <- 500

id <- sample(1:500)
set.seed(2017)
weight <- rnorm(n, mean = 150, sd = 26) # normal distributed
test.result <- rbinom(n, size = 1, prob = 0.4)
test.result <- factor(test.result, levels = c(0, 1))
egg.counts <- rpois(n, lambda = 7)
dat <- data.frame(id, weight, test.result, egg.counts)

head(dat)
str(dat)
summary(dat)

###################################################
### code chunk number 1: Brucellosis.sim.Rnw:66-67
###################################################
herd.id <- seq (1:75)


###################################################
### code chunk number 2: Brucellosis.sim.Rnw:71-76
###################################################
set.seed(1104)
# Sampling 50 times from the range of 3 to 30
# replace = TRUE means that the sampling is with replacement.
herdsize1 <- sample(3:30, 50, replace = TRUE)
set.seed(1104)
# Sampling 25 times from the range of 30 to 125
# replace = TRUE means that the sampling is with replacement.
herdsize2 <- sample(30:125, 25, replace = TRUE)
herd.size <- c(herdsize1, herdsize2)
length(herd.size)

###################################################
### code chunk number 3: Brucellosis.sim.Rnw:80-83
###################################################
set.seed(1104)
# Create the factor variable "disinfectant"
# Sampling 75 times from the vector c("Yes", "No")
# both "yes" and "no" are both sampled with a probability of 0.5
disinfectant <- factor(sample(c("Yes", "No"), 75, replace = TRUE,
                              prob = c(0.5, 0.5)), 
                       levels = c("Yes", "No")) 

###################################################
### code chunk number 4: Brucellosis.sim.Rnw:87-90
###################################################
set.seed(1104)
# sample subset from a herd
sampled <- sample(5:10, 75, replace = TRUE)  
table(sampled)


###################################################
### code chunk number 5: Brucellosis.sim.Rnw:94-95
###################################################
# Create data frame
length(herd.id)
length(herd.size)
length(sampled)
data <- data.frame(herd.id, herd.size, sampled, disinfectant)


###################################################
### code chunk number 6: Brucellosis.sim.Rnw:99-100
###################################################
# ifelse command: ifelse(test, yes, no)
# ifelse returns a value with the same shape as test which is filled with elements
# selected from either yes or no depending on whether the element of test is
# TRUE or FALSE.
data$Sampled.corr <- ifelse(data$sampled > data$herd.size,
                            data$herd.size, data$sampled) 


###################################################
### code chunk number 7: Brucellosis.sim.Rnw:104-111
###################################################
# from the 75 rows we would like to expand the data frame
# rep(...) replicates the values in x. 
expanded <- data.frame(herd.id = rep(data$herd.id, data$Sampled.corr),
                       herd.size = rep(data$herd.size, data$Sampled.corr),
                       disinfectant = factor(rep(data$disinfectant,
                                                 data$Sampled.corr), 
                                             levels = c("Yes", "No")))
head(data)
nrow(expanded[which(expanded$herd.id == 1), ])
nrow(expanded[which(expanded$herd.id == 2), ])
nrow(expanded[which(expanded$herd.id == 3), ])
str(expanded)
dim(expanded)
head(expanded)[c(1:20),]
summary(expanded)

###################################################
### code chunk number 8: Brucellosis.sim.Rnw:115-120
###################################################
m <- median(expanded$herd.size)
n <-  length(expanded[,1])
herd.size.dic <- ifelse(expanded$herd.size > m, 1, 0)  
herd.size.large <- ifelse(expanded$herd.size > m, 1, 0)  
herd.size.small <- ifelse(expanded$herd.size < m, 1, 0) 

###################################################
### code chunk number 9: Brucellosis.sim.Rnw:124-138
###################################################
set.seed(1104)
random.1 <- rbinom(n,1,0.99) 
Rose.Bengal.large <- random.1 * herd.size.large
Rose.Bengal.small <- random.1 * herd.size.small
Rose.Bengal.final <- Rose.Bengal.large
Rose.Bengal.final[which(Rose.Bengal.large == 0)] <- Rose.Bengal.small[which(Rose.Bengal.large == 0)]
Rose.Bengal.final <- factor(Rose.Bengal.final)
RB <- data.frame(Rose.Bengal.final, Rose.Bengal.large,
                 Rose.Bengal.small, herd.size.dic)
table(RB$Rose.Bengal.final,RB$Rose.Bengal.large)
table(RB$Rose.Bengal.final,RB$Rose.Bengal.small)
table(RB$Rose.Bengal.final, RB$herd.size.dic)
fisher.test(table(RB$Rose.Bengal.final, RB$herd.size.dic))
sum(Rose.Bengal.final)
head(RB)


###################################################
### code chunk number 10: Brucellosis.sim.Rnw:142-145
###################################################
newdat<- cbind.data.frame(expanded, Rose.Bengal.final)
str(newdat)
summary(newdat)


###################################################
### code chunk number 11: Brucellosis.sim.Rnw:149-151
###################################################
tapply(newdat$herd.size,newdat$Rose.Bengal.final,mean)
tapply(newdat$herd.size,newdat$Rose.Bengal.final,median)


###################################################
### code chunk number 12: Brucellosis.sim.Rnw:155-157
###################################################
m1 <- glm(Rose.Bengal.final ~ herd.size, data = newdat, family = "binomial")
summary(m1)


###################################################
### code chunk number 13: Brucellosis.sim.Rnw:161-166
###################################################
set.seed(1104)
random2<- rbinom(n,1,0.85) 
newdat$vaccine <- random2 * (as.numeric(newdat$Rose.Bengal.final)-1)
newdat$vaccine <- factor(newdat$vaccine, level=c("0", "1"),
                         label = c("yes", "no"))
table(newdat$vaccine)
str(newdat)
summary(newdat)

###################################################
### code chunk number 14: Brucellosis.sim.Rnw:170-172
###################################################
m2 <- glm(Rose.Bengal.final ~ vaccine, data = newdat, family = "binomial")
exp(coef(m2))
summary(m2)


