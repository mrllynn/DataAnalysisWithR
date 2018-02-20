
rm(list=ls())

library("rpart")
library("partykit")
library("party")
library("randomForest")
library("pROC")

# MURIEL
# lung <- read.csv("~/Dropbox/201710_Makerere/03_Exercises/data/perulung_ems.csv", sep = ";")
# SONJA
lung <- read.csv("C:\\Users\\admin\\Dropbox\\201710_Makerere\\03_Exercises\\data\\perulung_ems.csv",
                 sep = ";")
head(lung)
str(lung)
lung$sex <- factor(lung$sex,
                   levels = c(0, 1),
                   labels = c("female", "male"))
lung$respsymptoms <- factor(lung$respsymptoms,
                            levels = c(0, 1),
                            labels = c("no", "yes"))
str(lung)

# LM approach
# In exercise 16 we learnt, ...
lm.mod <- lm(fev1 ~ age + height + sex + respsymptoms, data = lung)
summary(lm.mod)
par(mfrow=c(2,2))
plot(lm.mod)

# GLM approach
glm.mod.0 <- glm(respsymptoms ~ fev1 + age + height + sex,
               family = binomial,
               data = lung)
summary(glm.mod.0)
exp(coef(glm.mod.0))

glm.mod.1 <- glm(respsymptoms ~ fev1 + age + height,
               family = binomial,
               data = lung)
summary(glm.mod.1)
exp(coef(glm.mod.1))
AIC(glm.mod.0, glm.mod.1)

# tree approach
# Compare the tree approaches to the glm(...)
tree.mod <- ctree(respsymptoms ~ fev1 + age + height + sex,
                data = lung)
plot(tree.mod, tp_args = list(id = FALSE))

# library("pROC")
# ?roc
# my.roc <- roc(lung$respsymptoms ~ lung$fev1) 
# coords(my.roc, "best")
# coords(my.roc, x="best",
#        input="threshold",
#        best.method="youden") # Same than last line
# par(mfrow=c(1,1))
# plot(my.roc)
# auc(my.roc)

glm.tree.mod <- glm(respsymptoms ~ fev1 + age,
                 family = binomial,
                 data = lung)

summary(glm.tree.mod)
predict(glm.tree.mod)
objects(glm.tree.mod)
exp(coef(glm.tree.mod))

# my.roc2 <- roc(lung$respsymptoms ~ lung$pred.glm) 
# coords(my.roc2, "best")
# coords(my.roc2, x="best",
#        input="threshold",
#        best.method="youden") # Same than last line
# par(mfrow=c(1,1))
# plot(my.roc2)
# auc(my.roc2)
