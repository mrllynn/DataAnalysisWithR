rm(list=ls())

library("MASS")
library("rpart")
library("partykit")
library("party")
library("randomForest")


data("bacteria")
str(bacteria)
bact <- data.frame(y = bacteria$y, hilo = bacteria$hilo, trt = bacteria$trt)
tree.2 <- ctree(y ~ ., data = bact)
plot(tree.2,tp_args = list(id = FALSE))

data("ToothGrowth")
str(ToothGrowth)
my.tooth.tree <- ctree(len ~ ., data = ToothGrowth)
plot(my.tooth.tree, tp_args = list(id = FALSE))

my.tooth.tree <- ctree(len ~ ., data = ToothGrowth,
                       controls = ctree_control(mincriterion = 0.95))
plot(my.tooth.tree, tp_args = list(id = FALSE))


library(TH.data)
str(bodyfat)
my.bodyfat.tree <- ctree(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth
                         + kneebreadth, data = bodyfat)
plot(my.bodyfat.tree, tp_args = list(id = FALSE))

my.bodyfat.tree <- ctree(DEXfat ~ ., data = bodyfat)
plot(my.bodyfat.tree, tp_args = list(id = FALSE))

my.bodyfat.tree <- ctree(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth
                         + kneebreadth, data = bodyfat,
                         controls = ctree_control(mincriterion = 0.65))
plot(my.bodyfat.tree, tp_args = list(id = FALSE))

my.bodyfat.tree <- ctree(DEXfat ~ ., data = bodyfat,
                         controls = ctree_control(mincriterion = 0.3))
plot(my.bodyfat.tree, tp_args = list(id = FALSE))



######################################################################
# Recursive partitioning: conditional inference tree
# In each node of those trees, a significance test on independence between 
# any of the covariates and the response is performed.
# A split is established when the p-value, possibly adjusted for 
# multiple comparisons, is smaller than a pre-specified nominal level alpha.

# In more details, for the first step, one selects a covariate (one of the 
# x-variables) and estimates a split point which separates the response values 
# of the y-variable into two groups.
# In a second step, when a split was established for one covariate, the 
# same procedure will be applied to the two groups. The recursion is stopped
# when some stopping criterion is applied.



