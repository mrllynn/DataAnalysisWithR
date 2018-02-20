#R code from vignette source 'Day4_Exercises_Solutions.Rnw'

###################################################
#code chunk number 1: Day4_Exercises_Solutions.Rnw:64-87 (eval = FALSE)
###################################################
?t.test
t.test(ToothGrowth$len ~ ToothGrowth$supp)
t.test(len ~ supp, data = ToothGrowth) 
# p-value = 0.06039 (borderline) significant, close to 0.05
# p-value says the difference is not (borderline) significant
# however, the boxplot do somehow look different
boxplot(ToothGrowth$len ~ ToothGrowth$supp)
# change the default setting of var.equal
t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal = TRUE)
t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal = FALSE) # DEFAULT!

# Define subset
sub.OJ <- subset(ToothGrowth, supp == "OJ")
sub.VC <- subset(ToothGrowth, supp == "VC")
# Drop (unused) levels for each subset
sub.VC$supp <- droplevels(sub.VC$supp)
levels(sub.VC$supp) # check that levels are dropped
sub.OJ$supp <- droplevels(sub.OJ$supp)
levels(sub.OJ$supp) # check that levels are dropped
# Additional option for comparing lengths between the two groups:
# Compare the two vectors of lengths
t.test(sub.VC$len, sub.OJ$len)



###################################################
#code chunk number 2: Day4_Exercises_Solutions.Rnw:90-99 (eval = FALSE)
###################################################
# t = 1.9153
# df = 55.309
# p-value = 0.06063
# 95 percent confidence interval: -0.1710156  7.5710156
# sample mean in group OJ: 20.66333         
# sample mean in group VC: 16.96333
# Also with the lm(...) function  for "linear model" you get the same sample means:
lm.mod0 <- lm(ToothGrowth$len ~ ToothGrowth$supp - 1)
coef(lm.mod)


###################################################
#code chunk number 3: Day4_Exercises_Solutions.Rnw:102-128 (eval = FALSE)
###################################################
# two-sided t-test of fev1 vs respsymptoms
t.test(lung$fev1 ~ lung$respsymptoms)
t.test(fev1 ~ respsymptoms, data = lung)
# Define linear model
mod.fev.resp.0 <- lm(lung$fev1 ~ lung$respsymptoms)
summary(mod.fev.resp.0)
mod.fev.resp.1 <- lm(lung$fev1 ~ lung$respsymptoms - 1)
summary(mod.fev.resp.1)
# Coefficients of linear model
coef(mod.fev.resp.0)
coef(mod.fev.resp.1)
# Anova
anova(mod.fev.resp.0)
anova(mod.fev.resp.1)
# two-sided t-test of fev1 vs sex
t.test(lung$fev1 ~ lung$sex)
t.test(fev1 ~ sex, data = lung)
# Define linear model
mod.fev.sex.0 <- lm(lung$fev1 ~ lung$sex)
mod.fev.sex.1 <- lm(lung$fev1 ~ lung$sex - 1)
# Coefficients of linear model
coef(mod.fev.sex.0)
coef(mod.fev.sex.1)
# Anova
anova(mod.fev.sex.0)
anova(mod.fev.sex.1)


###################################################
#code chunk number 4: Day4_Exercises_Solutions.Rnw:137-158 (eval = FALSE)
###################################################
library("MASS")
data(bacteria)
summary(bacteria)
subbac <- subset(bacteria, week == 2)
bacteria$trt.new <- bacteria$trt
levels(bacteria$trt.new) <- c("placebo", "drug", "drug")
bacteria$trt.new <- droplevels(bacteria$trt.new)
# Ordering of the variables does not matter
chisq.test(table(bacteria$trt, bacteria$y))
chisq.test(table(bacteria$y, bacteria$trt))
chisq.test(bacteria$trt, bacteria$y)
my.table <- table(bacteria$trt, bacteria$y)
chisq.test(my.table) 
table(subbac$trt, subbac$y)
chisq.test(table(subbac$trt, subbac$y))
fisher.test(table(subbac$trt, subbac$y))
fisher.test(table(subbac$trt.new, subbac$y))
# Chi-squared test with trt and y
chisq.test(table(bacteria$trt, bacteria$y))
# Fisher test with trt and y
fisher.test(table(bacteria$trt, bacteria$y))


###################################################
#code chunk number 5: Day4_Exercises_Solutions.Rnw:163-169 (eval = FALSE)
###################################################
subbac <- subset(bacteria, week == 2)
# Chi-squared test with trt and y
chisq.test(table(subbac$trt, subbac$y))
# --> NOT RELIABLE RESULTS: at least 5 observations per group.
# Fisher test with trt and y
fisher.test(table(subbac$trt, subbac$y))


###################################################
#code chunk number 6: Day4_Exercises_Solutions.Rnw:173-184 (eval = FALSE)
###################################################
# WHOLE DATA SET
# Chi-squared test with trt.new and y
chisq.test(table(bacteria$trt.new, bacteria$y))
# Fisher test with trt.new and y
fisher.test(table(bacteria$trt.new, bacteria$y))
# SUB DATA SET only observations from week 2
# Chi-squared test with trt.new and y
chisq.test(table(subbac$trt.new, subbac$y))
# --> NOT RELIABLE RESULTS: at least 5 observations per group.
# Fisher test with trt.new and y
fisher.test(table(subbac$trt.new, subbac$y))


###################################################
#code chunk number 7: Day4_Exercises_Solutions.Rnw:187-194 (eval = FALSE)
###################################################
fisher.test(table(subbac$trt.new, subbac$y))
fisher.test(bacteria$y, bacteria$ap)
my.logreg <- glm(y ~ ap, data = bacteria, family = "binomial")
summary(my.logreg)
exp(0.8473 )
coef(my.logreg)
exp(coef(my.logreg))


###################################################
#code chunk number 8: Day4_Exercises_Solutions.Rnw:197-203 (eval = FALSE)
###################################################
model.logreg <- glm(bacteria$y ~ bacteria$trt.new, family = "binomial")
model.logreg <- glm(y ~ trt.new, data = bacteria, family = "binomial")
summary(model.logreg)
anova(model.logreg)
coef(model.logreg)
exp(coef(model.logreg))


###################################################
#code chunk number 9: Day4_Exercises_Solutions.Rnw:211-214 (eval = FALSE)
###################################################
# After the demonstration us the following commands:
dev.off()
par(mfrow=c(1,1))


###################################################
#code chunk number 10: Day4_Exercises_Solutions.Rnw:218-220 (eval = FALSE)
###################################################
boxplot(ToothGrowth$len, xlab = "Length of Teeth",
        ylab = "Length in mm")


###################################################
#code chunk number 11: Day4_Exercises_Solutions.Rnw:223-231 (eval = FALSE)
###################################################
# OPTION 1:
boxplot(ToothGrowth$len, xlab = "Length of Teeth",
        ylab = "Length in mm",
        main = "Boxplot of Tooth Length")
# OPTION 2:
boxplot(ToothGrowth$len, xlab = "Length of Teeth",
        ylab = "Length in mm")
title("Boxplot of Tooth Length")


###################################################
#code chunk number 12: Day4_Exercises_Solutions.Rnw:234-235 (eval = FALSE)
###################################################
par(mfrow=c(2,2))


###################################################
#code chunk number 13: Day4_Exercises_Solutions.Rnw:237-244 (eval = FALSE)
###################################################
# With the par(...) function, you can include the option
# mfrow=c(nrows, ncols) to create a matrix of nrows x ncols plots
# that are filled in by row.
par(mfrow=c(2,2)) # 2 rows, 2 columns
par(mfrow=c(4,3)) # 4 rows, 3 columns
# DO NOT FORGET TO CHANGE IT BACK TO:
par(mfrow=c(1, 1)) # the default


###################################################
#code chunk number 14: Day4_Exercises_Solutions.Rnw:250-254 (eval = FALSE)
###################################################
sub.casein <- subset(chickwts, feed == "casein")
sub.casein <- droplevels(sub.casein)
sub.horsebean <- subset(chickwts, feed == "horsebean")
sub.horsebean <- droplevels(sub.horsebean)


###################################################
#code chunk number 15: Day4_Exercises_Solutions.Rnw:256-264 (eval = FALSE)
###################################################
sub.casein <- subset(chickwts, feed == "casein")
sub.casein <- droplevels(sub.casein)
sub.horsebean <- subset(chickwts, feed == "horsebean")
sub.horsebean <- droplevels(sub.horsebean)
summary(sub.casein$weight)
summary(sub.horsebean$weight)
boxplot(sub.casein$weight ~ sub.casein$feed, ylim = c(100, 410))
boxplot(sub.horsebean$weight ~ sub.horsebean$feed, ylim = c(100, 410))


###################################################
#code chunk number 16: Day4_Exercises_Solutions.Rnw:268-269 (eval = FALSE)
###################################################
plot(lung$fev1, lung$height)


###################################################
#code chunk number 17: Day4_Exercises_Solutions.Rnw:271-273 (eval = FALSE)
###################################################
plot(lung$fev1, lung$height, cex.axis = 1.5, cex.lab = 1.5)
plot(lung$fev1, lung$height, cex.axis = 1.5, cex.lab = 1.5, las = 1)


###################################################
#code chunk number 18: Day4_Exercises_Solutions.Rnw:277-278 (eval = FALSE)
###################################################
plot(ToothGrowth$dose, ToothGrowth$len)


###################################################
#code chunk number 19: Day4_Exercises_Solutions.Rnw:280-282 (eval = FALSE)
###################################################
plot(ToothGrowth$dose, ToothGrowth$len,
     xlab = expression(paste("Vitamin C in ", mu, "g")))


###################################################
#code chunk number 20: Day4_Exercises_Solutions.Rnw:292-294 (eval = FALSE)
###################################################
barplot(prop.table(table(bacteria$y, bacteria$trt),margin=1),
        beside=FALSE, ylim = c(0,0.8))


###################################################
#code chunk number 21: Day4_Exercises_Solutions.Rnw:296-304 (eval = FALSE)
###################################################
barplot(prop.table(table(bacteria$y, bacteria$trt),margin=1), beside=FALSE,
        ylim = c(0,0.8), legend.text = levels(bacteria$y))
# Helen's solution (THANK YOU!):
barplot(prop.table(table(bacteria$y, bacteria$trt), margin=1),
        beside=FALSE, ylim = c(0,0.8), col = topo.colors(2),
        ylab = "y", xlab = "treatments",
        main = "bacteria")
legend("topright", legend = c("yes", "no"), fill = topo.colors(2))


###################################################
#code chunk number 22: Day4_Exercises_Solutions.Rnw:307-308 (eval = FALSE)
###################################################
hist(ToothGrowth$len, prob = TRUE, col = "grey", ylim = c(0, 0.05))


###################################################
#code chunk number 23: Day4_Exercises_Solutions.Rnw:310-316 (eval = FALSE)
###################################################
hist(ToothGrowth$len, prob = TRUE, col = "grey", ylim = c(0, 0.05))
# add a density estimate with defaults
lines(density(ToothGrowth$len), col="blue", lwd = 4) 
# add a density estimate with adjustments
lines(density(ToothGrowth$len, adjust=2), lty="dotted", col="darkgreen",
      lwd = 4) 


###################################################
#code chunk number 24: Day4_Exercises_Solutions.Rnw:319-320 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1)


###################################################
#code chunk number 25: Day4_Exercises_Solutions.Rnw:322-336 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1)
abline(lm(lung$fev1 ~ lung$height), col = "red",
       lwd = 3, lty = 2)
# See
# https://stackoverflow.com/questions/24173468/r-print-equation-of-linear-regression-on-the-plot-itself
# to learn how to print equation of linear regression on the plot
rounded coefficients for better output
lm.mod <- lm(lung$fev1 ~ lung$height)
cf <- round(coef(lm.mod), 2) 
sign check to avoid having plus followed by minus for negative coefficients
eq <- paste0("fev1 = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " height ")
printing of the equation
mtext(eq, 3, line=-2)


###################################################
#code chunk number 26: Day4_Exercises_Solutions.Rnw:339-340 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1)


###################################################
#code chunk number 27: Day4_Exercises_Solutions.Rnw:342-343 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1, col = as.numeric(lung$sex))


###################################################
#code chunk number 28: Day4_Exercises_Solutions.Rnw:346-347 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1)


###################################################
#code chunk number 29: Day4_Exercises_Solutions.Rnw:349-359 (eval = FALSE)
###################################################
plot(lung$height, lung$fev1, col = as.numeric(lung$respsymptoms))
abline(lm(lung$fev1 ~ lung$height,
          data = subset(lung, sex == "female")),
       col  = "black")
abline(lm(lung$fev1 ~ lung$height,
          data = subset(lung, sex == "male")),
       col  = "red")
# library("graphics")
# coplot(fev1 ~ height | sex, data = lung, panel = panel.smooth)
# coplot(fev1 ~ height | respsymptoms, data = lung, panel = panel.smooth)


###################################################
#code chunk number 30: Day4_Exercises_Solutions.Rnw:363-364 (eval = FALSE)
###################################################
plot(ToothGrowth$len, ToothGrowth$dose)


###################################################
#code chunk number 31: Day4_Exercises_Solutions.Rnw:366-369 (eval = FALSE)
###################################################
plot(ToothGrowth$len, ToothGrowth$dose,
     pch = levels(ToothGrowth$supp),
     col = as.numeric(ToothGrowth$supp))


