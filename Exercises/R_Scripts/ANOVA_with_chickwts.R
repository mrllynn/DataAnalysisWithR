### R code from 'ANOVA_with_chickwts.Rnw'

data(chickwts)
str(chickwts)
head(chickwts)
boxplot(chickwts$weight ~ chickwts$feed)

# aov.mod <- aov(chickwts$weight ~ chickwts$feed)
aov.mod <- aov(weight ~ feed, data = chickwts)
# What objects can we extract from a anova model?
objects(aov.mod)
#
summary(aov.mod)

# What are residuals?
chickwts$residuals <- residuals(aov.mod)
tapply(chickwts$weight, chickwts$feed, mean)
chickwts[c(1:3),]
# Save residuals to an objects and check mean of residuals
aov.mod.resid <- residuals(aov.mod)
mean(aov.mod.resid)

par(mfrow=c(1,1))
qqnorm(aov.mod.resid)
qqline(aov.mod.resid, col = "red", lwd = 3, lty = 2)
# Shapiro-Wilk test (dependent on sample size --> limited use)
shapiro.test(aov.mod.resid)

# Bartlett Test
bartlett.test(chickwts$weight ~ chickwts$feed)
# Levene's Test
# library("Rcmdr")
# levene.test(chickwts$weight ~ chickwts$feed)

# Plot fitted against residual values
plot(fitted.values(aov.mod), residuals(aov.mod))

# Plot fitted against residual values
par(mfrow=c(1,2), pty="s", mar = c(1, 4, 1, 2))
plot(fitted.values(aov.mod), residuals(aov.mod))
abline(h = 0, col = "red", lwd = 3, lty = 2)
plot(aov.mod, which=1)

# Plot fitted against residual values
# Cut-off at 3
par(mfrow=c(1,1), pty="s", mar = c(5, 4, 4, 2))
plot(aov.mod, which=4)

par(mfrow=c(1,3), pty="s")
plot(fitted.values(aov.mod), residuals(aov.mod))
abline(h = 0, col = "red", lwd = 3, lty = 2)
# Plot residuals against variables from the model
plot(chickwts$weight, residuals(aov.mod), ylab = "residuals")
plot(chickwts$feed, residuals(aov.mod),
     xlab = "chickwts$feed", ylab = "residuals")

par(mfrow=c(2, 2))
plot(aov.mod)

# HOW TO RELEVEL FACTORS?
# How to change the reference category of a factor variable?
# Use the relevel(...) function
# Make "sunflower" as reference category
chickwts$feed <- relevel(chickwts$feed, "sunflower")
levels(chickwts$feed)
# Make "linseed" as reference category
chickwts$feed <- relevel(chickwts$feed, "linseed")
levels(chickwts$feed)

aov.mod <- aov(weight ~ feed, data = chickwts)
pairwise.t.test(chickwts$weight, chickwts$feed, p.adj = "none")
pairwise.t.test(chickwts$weight, chickwts$feed, p.adj = "bonferroni")

install.packages("multcomp")
library("multcomp")
# compares always to baseline levels (here: casein) --> saves degrees of freedom
dunnett <- glht(aov.mod, linfct = mcp(feed = "Dunnett"))
summary(dunnett)

library("multcomp")
# compares all factor levels
tukey <- glht(aov.mod, linfct = mcp(feed = "Tukey"))
summary(tukey)
# summary(tukey)          # standard display
tukey.cld <- cld(tukey)   # letter-based display
# the cld(...) function sets up a compact letter display of all pair-wise comparisons
par(mfrow=c(1,1))
plot(tukey.cld)
