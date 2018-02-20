# R code from 'Day5_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day5_Exercises_Solutions.Rnw:66-72 (eval = FALSE)
###################################################
# install.packages("HSAUR3")
library("HSAUR3")
data("water")
str(water)
head(water)
summary(water)


###################################################
#code chunk number 2: Day5_Exercises_Solutions.Rnw:76-79 (eval = FALSE)
###################################################
par(mfrow=c(1,1))
plot(x = water$hardness, y = water$mortality)
plot(mortality ~ hardness, data = water)


###################################################
#code chunk number 3: Day5_Exercises_Solutions.Rnw:83-87 (eval = FALSE)
###################################################
plot(x = water$hardness, y = water$mortality, 
     main = "Calcium concentration against mortality")
plot(mortality ~ hardness, data = water,
     main = "Calcium concentration against mortality")


###################################################
#code chunk number 4: Day5_Exercises_Solutions.Rnw:96-109 (eval = FALSE)
###################################################
# cex: number indicating the amount by which plotting text and symbols
# should be scaled
# cex.axis:	magnification of axis annotation relative to cex
plot(x = water$hardness, y = water$mortality,
     cex.axis = 1.5, # (1) enlarge number of the axis
     cex.lab = 1.5, # (2) enlarge font size of axis labels
     cex = 1.5, # (3) enlarge point size within plot
     main = "Calcium concentration vs. mortality")
plot(mortality ~ hardness, data = water,
     cex.axis = 1.5, # enlarge number of the axis
     cex = 1.5, # enlarge point size within plot
     cex.lab = 1.5, # enlarge font size of axis labels
     main = "Calcium concentration vs. mortality")


###################################################
#code chunk number 5: Day5_Exercises_Solutions.Rnw:115-121 (eval = FALSE)
###################################################
cor(x = water$hardness, y = water$mortality) # -0.6548486
cor.test(x = water$hardness, y = water$mortality)
# negative correlation of -0.65 with confidence interval of [-0.78, -0.48]:
# the higher the calcium concentration (hardness),
# the smaller the averaged annual mortality per 100.000 male
# inhabitants (mortality)


###################################################
#code chunk number 6: Day5_Exercises_Solutions.Rnw:126-132 (eval = FALSE)
###################################################
plot(x = water$hardness, y = water$mortality,
     col = as.numeric(water$location),
     pch = 16, cex.axis = 1.5,
     cex = 1.5, cex.lab = 1.5)
library("graphics")
coplot(mortality ~ hardness | location, data = water, panel = panel.smooth)


###################################################
#code chunk number 7: Day5_Exercises_Solutions.Rnw:136-142 (eval = FALSE)
###################################################
plot(x = water$hardness, y = water$mortality,
     col = as.numeric(water$location),
     pch = 16, cex.axis = 1.5,
     cex = 1.5, cex.lab = 1.5)
legend("topright", legend = levels(water$location),
       col = c("black", "red"), pch = 16, cex = 1.5)


###################################################
#code chunk number 8: Day5_Exercises_Solutions.Rnw:146-147 (eval = FALSE)
###################################################
barplot(table(water$location))


###################################################
#code chunk number 9: Day5_Exercises_Solutions.Rnw:151-160 (eval = FALSE)
###################################################
install.packages("graphics")
library("graphics")
?coplot
#
# install.packages("lattice")
library("lattice")
?xyplot
#
?interaction.plot


###################################################
#code chunk number 10: Day5_Exercises_Solutions.Rnw:162-174 (eval = FALSE)
###################################################
# PERULUNG DATA SET
coplot(fev1 ~ height | sex, data = lung, panel = panel.smooth)
coplot(fev1 ~ height | respsymptoms, data = lung, panel = panel.smooth)

xyplot(fev1 ~ height | sex, data = lung)
xyplot(fev1 ~ height | respsymptoms, data = lung)

# ToothGrowth DATA SET
interaction.plot(ToothGrowth$dose,
                 ToothGrowth$supp,
                 ToothGrowth$len,
                 fixed = TRUE)


###################################################
#code chunk number 11: Day5_Exercises_Solutions.Rnw:187-193 (eval = FALSE)
###################################################
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
ToothGrowth$dose.fac <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0),
                               labels = c("low", "med", "high"))
table(ToothGrowth$dose.fac)


###################################################
#code chunk number 12: Day5_Exercises_Solutions.Rnw:196-197 (eval = FALSE)
###################################################
boxplot(ToothGrowth$len ~ ToothGrowth$dose.fac)


###################################################
#code chunk number 13: Day5_Exercises_Solutions.Rnw:202-315 (eval = FALSE)
###################################################
# aov.mod <- aov(ToothGrowth$len ~ ToothGrowth$dose.fac)
aov.mod <- aov(len ~ dose.fac, data = ToothGrowth)
# What objects can we extract from a anova model?
objects(aov.mod)
#
summary(aov.mod)

# What are residuals?
ToothGrowth$residuals <- residuals(aov.mod)
tapply(ToothGrowth$len, ToothGrowth$dose.fac, mean)
ToothGrowth[c(1:3),]
# Save residuals to an objects and check mean of residuals
aov.mod.resid <- residuals(aov.mod)
mean(aov.mod.resid)
round(mean(aov.mod.resid), 3)

par(mfrow=c(1,1))
qqnorm(aov.mod.resid)
qqline(aov.mod.resid, col = "red", lwd = 3, lty = 2)
# Shapiro-Wilk test (dependent on sample size --> limited use)
shapiro.test(aov.mod.resid)
# a <- rnorm(100, 20, 3)
# qqnorm(a)
# qqplot(a)
# shapiro.test(a)

# Bartlett Test
bartlett.test(ToothGrowth$len ~ ToothGrowth$dose.fac)

# Levene's Test
# install.packages("Rcmdr")
# library("Rcmdr")
# levene.test(ToothGrowth$len ~ ToothGrowth$dose.fac)

# Plot fitted against residual values
objects(aov.mod)
plot(fitted.values(aov.mod), residuals(aov.mod))

# Plot fitted against residual values
par(mfrow=c(1,2), pty="s", mar = c(1, 4, 1, 2))
plot(fitted.values(aov.mod), residuals(aov.mod))
abline(h = 0, col = "red", lwd = 3, lty = 2)
plot(aov.mod, which=1)

# Plot fitted against residual values
# Cut-off at 3 (y-axis)

# observations above 3 are regarded as having high
# influence to the analysis - have a closer look at them:
# outliers? delete them from the data set?
# why are these observations so influencial?
# everything below 3 is okay for the model
par(mfrow=c(1,1), pty="s", mar = c(5, 4, 4, 2))
plot(aov.mod, which=4)
# ToothGrowth[c(22, 23, 32),]

par(mfrow=c(1,3), pty="s")
plot(fitted.values(aov.mod), residuals(aov.mod))
abline(h = 0, col = "red", lwd = 3, lty = 2)
# Plot residuals against variables from the model
plot(ToothGrowth$len, residuals(aov.mod), ylab = "residuals")
plot(ToothGrowth$dose.fac, residuals(aov.mod),
     xlab = "ToothGrowth$dose.fac", ylab = "residuals")

par(mfrow=c(2, 2))
plot(aov.mod)

# # HOW TO RELEVEL FACTORS?
# # How to change the reference category of a factor variable?
# # Use the relevel(...) function
# # Make "sunflower" as reference category
# chickwts$feed <- relevel(chickwts$feed, "sunflower")
# levels(chickwts$feed)
# # Make "linseed" as reference category
# chickwts$feed <- relevel(chickwts$feed, "linseed")
# levels(chickwts$feed)
# chickwts$feed <- relevel(chickwts$feed, "casein")
# levels(chickwts$feed)

aov.mod <- aov(len ~ dose.fac, data = ToothGrowth)

# aov.mod1 <- aov(len ~ dose.fac, data = ToothGrowth)
# aov.mod2 <- aov(ToothGrowth$len ~ ToothGrowth$dose.fac)
# summary(aov.mod1)
# summary(aov.mod2)

# DO NOT USE THIS COMMAND, OTHERWISE THE LINEAR FUNCTION WITHIN
# DUNNETT AND TUKEY DOES NOT WORK!
# --> specify the data at the end of the aov model
# aov.mod <- aov(ToothGrowth$len ~ ToothGrowth$dose.fac)
summary(aov.mod)
pairwise.t.test(ToothGrowth$len, ToothGrowth$dose.fac, p.adj = "none")
pairwise.t.test(ToothGrowth$len, ToothGrowth$dose.fac, p.adj = "bonferroni")

# install the package first (one time)
# install.packages("multcomp") 
# load the library (every single time you use it!)
library("multcomp")
# compares always to baseline levels (here: casein) --> saves degrees of freedom
# make sure you saved the aov.mod as:
# aov.mod <- aov(len ~ dose.fac, data = ToothGrowth)
dunnett <- glht(aov.mod, linfct = mcp(dose.fac = "Dunnett"))
summary(dunnett)

library("multcomp")
# compares all factor levels
tukey <- glht(aov.mod, linfct = mcp(dose.fac = "Tukey"))
summary(tukey)
# summary(tukey)          # standard display
tukey.cld <- cld(tukey)   # letter-based display
# the cld(...) function sets up a compact letter display of all pair-wise comparisons
par(mfrow=c(1,1), mar=c(5,4,8,2))
plot(tukey.cld)


###################################################
#code chunk number 14: Day5_Exercises_Solutions.Rnw:327-391 (eval = FALSE)
###################################################
lung <- read.csv("~/Dropbox/data/perulung_ems.csv", sep = ";")
head(lung)
str(lung)
lung$sex <- factor(lung$sex, levels = c("0", "1"))
levels(lung$sex) <- c("female", "male")
lung$respsymptoms <- factor(lung$respsymptoms, levels = c("0", "1"))

# MODEL 1
# mod.age <- lm(fev1 ~ age, data = lung)
mod.age <- lm(lung$fev1 ~ lung$age)
summary(mod.age)
coef(mod.age)
# Check model assumptions graphically
par(mfrow=c(2,2))
plot(mod.age)

# MODEL 2
# mod.height <- lm(fev1 ~ height, data = lung)
mod.height <- lm(lung$fev1 ~ lung$height)
summary(mod.height)
coef(mod.height)
# Check model assumptions graphically
par(mfrow=c(2,2))
plot(mod.height)

# MODEL 3
mod.age.height <- lm(fev1 ~ age + height, data = lung)
summary(mod.age.height)
coef(mod.age.height)
# Check model assumptions graphically
par(mfrow=c(2,2))
plot(mod.age.height)

# MODEL 4
mod.age.height.sex <- lm(fev1 ~ age + height + sex, data = lung)
summary(mod.age.height.sex)
coef(mod.age.height.sex)
# Check model assumptions graphically
par(mfrow=c(2,2))
plot(mod.age.height.sex)

# MODEL 5
mod.age.height.sex.resp <- lm(fev1 ~ age + height + sex + respsymptoms,
                              data = lung)
summary(mod.age.height.sex.resp)
coef(mod.age.height.sex.resp)
# Check model assumptions graphically
par(mfrow=c(2,2))
plot(mod.age.height.sex.resp)

mod1 <- lm(lung$fev1 ~ lung$age)
mod2 <- lm(lung$fev1 ~ lung$height)
mod3 <- lm(fev1 ~ age + height, data = lung)
mod4 <- lm(fev1 ~ age + height + sex, data = lung)
mod5 <- lm(fev1 ~ age + height + sex + respsymptoms,
           data = lung)
summary(mod5)

# MODEL SELECTION
AIC(mod1, mod2, mod3, mod4, mod5)
round(AIC(mod1, mod2, mod3, mod4, mod5), 2)
# Which of the models is best?
par(mfrow=c(2,2))
plot(mod5)


