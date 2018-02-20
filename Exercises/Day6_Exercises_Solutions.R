#R code from vignette source 'Day6_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day6_Exercises_Solutions.Rnw:66-69
###################################################
data(ToothGrowth)
ToothGrowth$dose.fac <- factor(ToothGrowth$dose,
                               levels = c(0.5, 1.0, 2.0),
                               labels = c("low", "med", "high"))


###################################################
#code chunk number 2: Day6_Exercises_Solutions.Rnw:72-75 (eval = FALSE)
###################################################
mod1 <- lm(len ~ dose.fac, data = ToothGrowth)
mod2 <- lm(len ~ supp, data = ToothGrowth)
mod3 <- lm(len ~ dose.fac + supp, data = ToothGrowth)


###################################################
#code chunk number 3: Day6_Exercises_Solutions.Rnw:78-93 (eval = FALSE)
###################################################
mod1 <- lm(len ~ dose.fac, data = ToothGrowth)
mod2 <- lm(len ~ supp, data = ToothGrowth)
mod3 <- lm(len ~ dose.fac + supp, data = ToothGrowth)
# mod4 <- lm(len ~ dose.fac + supp + dose.fac:supp, data = ToothGrowth)
mod4 <- lm(len ~ dose.fac*supp, data = ToothGrowth)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
# Check model assumptions
par(mfrow=c(2, 2))
plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)


###################################################
#code chunk number 4: Day6_Exercises_Solutions.Rnw:97-101 (eval = FALSE)
###################################################
AIC(mod1, mod2, mod3, mod4)
# t.test(ToothGrowth$len ~ ToothGrowth$supp) # not significant
# mod4 is the best model, because it has the smallest AIC.
# THE SMALLER THE AIC, THE BETTER THE MODEL!


###################################################
#code chunk number 5: Day6_Exercises_Solutions.Rnw:110-117 (eval = FALSE)
###################################################
library("HSAUR3")
data("water")
str(water)
head(water)
lm.mod <- lm(mortality ~ hardness + location,
             data = water)
summary(lm.mod)


###################################################
#code chunk number 6: Day6_Exercises_Solutions.Rnw:120-122 (eval = FALSE)
###################################################
par(mfrow=c(2,2))
plot(lm.mod)


###################################################
#code chunk number 7: Day6_Exercises_Solutions.Rnw:126-135 (eval = FALSE)
###################################################
lm.mod.int <- lm(mortality ~ hardness + location + hardness:location,
                 data = water)
summary(lm.mod.int)

mod1 <- lm(mortality ~ hardness + location,
           data = water)
mod2 <- lm(mortality ~ hardness + location + hardness:location,
           data = water)
AIC(mod1, mod2)


###################################################
#code chunk number 8: Day6_Exercises_Solutions.Rnw:139-141 (eval = FALSE)
###################################################
par(mfrow=c(2,2))
plot(lm.mod.int)


###################################################
#code chunk number 9: Day6_Exercises_Solutions.Rnw:144-147 (eval = FALSE)
###################################################
AIC(lm.mod, lm.mod.int)
summary(lm.mod)
summary(lm.mod.int)


###################################################
#code chunk number 10: Day6_Exercises_Solutions.Rnw:151-152 (eval = FALSE)
###################################################
confint(lm.mod)


###################################################
#code chunk number 11: Day6_Exercises_Solutions.Rnw:162-170 (eval = FALSE)
###################################################
lepto <- read.csv("~/Dropbox/201710_Makerere/03_Exercises/data/lepto.csv", sep = ";")
# SONJA
# lepto <- read.csv("C:\\Users\\admin\\Dropbox\\201710_Makerere\\03_Exercises\\data\\lepto.csv",
# sep = ";")
head(lepto)
str(lepto)




###################################################
#code chunk number 12: Day6_Exercises_Solutions.Rnw:173-186 (eval = FALSE)
###################################################
table(lepto$antibodies)
class(lepto$antibodies)
lepto$antibodies <- factor(lepto$antibodies, level = c(0, 1),
                           labels = c("no", "yes"))
# Many different ways how to encode a numeric variable into a factor:
# lepto$antibodies <- factor(lepto$antibodies,
#                            levels = c(0, 1),
#                            labels = c("no", "yes"))
# lepto$antibodies <- factor(lepto$antibodies,
#                            levels = c(0, 1),
#                            labels = c("NO", "YES"))
table(lepto$antibodies)
class(lepto$antibodies)


###################################################
#code chunk number 13: Day6_Exercises_Solutions.Rnw:189-190 (eval = FALSE)
###################################################
table(lepto$exposure, lepto$antibodies)


###################################################
#code chunk number 14: Day6_Exercises_Solutions.Rnw:194-201 (eval = FALSE)
###################################################
chisq.test(lepto$exposure, lepto$antibodies)
fisher.test(lepto$exposure, lepto$antibodies)
# fisher.test(table(lepto$exposure, lepto$antibodies))
glm.mod <- glm(antibodies ~ exposure, data = lepto,
               family = "binomial")
summary(glm.mod)
confint(glm.mod)


###################################################
#code chunk number 15: Day6_Exercises_Solutions.Rnw:205-207 (eval = FALSE)
###################################################
lepto.fem <- subset(lepto, gender == "female")
lepto.male <- subset(lepto, gender == "male")


###################################################
#code chunk number 16: Day6_Exercises_Solutions.Rnw:211-227 (eval = FALSE)
###################################################
# FEMALES
table(lepto.fem$exposure, lepto.fem$antibodies)
chisq.test(lepto.fem$exposure, lepto.fem$antibodies)
fisher.test(lepto.fem$exposure, lepto.fem$antibodies)
glm.mod.fem <- glm(antibodies ~ exposure, data = lepto.fem,
                   family = "binomial")
summary(glm.mod.fem)
confint(glm.mod.fem)
# MALES
table(lepto.male$exposure, lepto.male$antibodies)
chisq.test(lepto.male$exposure, lepto.male$antibodies)
fisher.test(lepto.male$exposure, lepto.male$antibodies)
glm.mod.male <- glm(antibodies ~ exposure, data = lepto.male,
                    family = "binomial")
summary(glm.mod.male)
confint(glm.mod.male)


###################################################
#code chunk number 17: Day6_Exercises_Solutions.Rnw:234-241 (eval = FALSE)
###################################################
glm.mod.final <- glm(antibodies ~ exposure + gender, data = lepto,
                     family = "binomial")
summary(glm.mod.final)
# Check that exposure and gender is also associated.
glm.exp.gen <- glm(exposure ~  gender, data = lepto,
                   family = "binomial")
summary(glm.exp.gen)


