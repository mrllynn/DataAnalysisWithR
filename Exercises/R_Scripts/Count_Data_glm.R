# # GLM Analysis ------------------------------------------------------------
# 
# rm(list=ls())
# ?bioChemists
# ## logit-poisson
# ## "art ~ ." is the same as "art ~ . | .", i.e.
# ## "art ~ fem + mar + kid5 + phd + ment | fem + mar + kid5 + phd + ment"
# fm.hp1 <- hurdle(art ~ ., data = bioChemists)
# summary(fm.hp1)
# 
# ## geometric-poisson
# fm.hp2 <- hurdle(art ~ ., data = bioChemists, zero = "geometric")
# summary(fm.hp2)
# 
# ## logit and geometric model are equivalent
# coef(fm.hp1, model = "zero") - coef(fm.hp2, model = "zero")
# 
# ## logit-negbin
# fm.hnb1 <- hurdle(art ~ ., data = bioChemists, dist = "negbin")
# summary(fm.hnb1)
# 
# ## negbin-negbin
# ## (poorly conditioned zero hurdle, note the standard errors)
# fm.hnb2 <- hurdle(art ~ ., data = bioChemists, dist = "negbin", zero = "negbin")
# summary(fm.hnb2)


# NMES1988 Analysis -------------------------------------------------------

rm(list=ls())

# install.packages("AER")
# install.packages("MASS")
# install.packages("pscl")

library("AER")
library("MASS")
library("pscl")

## select variables for analysis
data("NMES1988")
?NMES1988
dim(NMES1988)
nmes <- NMES1988[, c(1, 7:8, 13, 15)]
str(nmes)
# VARIABLES
# - visits: Number of physician office visits.
# - health: Factor indicating self-perceived health status,
#   levels are "poor", "average" (reference category), "excellent".
# - chronic: Number of chronic conditions.
# - gender: Factor indicating gender.
# - school:Number of years of education

## dependent variable
par(mfrow=c(1,1))
hist(nmes$visits, col = "darkgrey")

str(nmes)

table(nmes$health)
barplot(table(nmes$chronic))
barplot(table(nmes$gender))
barplot(table(nmes$school))

## Poisson regression
nmes.pois <- glm(visits ~ ., data = nmes, family = poisson)
summary(nmes.pois)
exp(coef(nmes.pois))

# ## LM test for overdispersion
# dispersiontest(nmes.pois)
# dispersiontest(nmes.pois, trafo = 2)
# ?dispersiontest

## sandwich covariance matrix
# coeftest(nmes.pois, vcov = sandwich)

## quasipoisson model
nmes.qpois <- glm(visits ~ ., data = nmes, family = quasipoisson)
summary(nmes.qpois)
exp(coef(nmes.qpois))

## NegBin regression
nmes.nb <- glm.nb(visits ~ ., data = nmes)
summary(nmes.nb)
exp(coef(nmes.nb))

## hurdle regression
nmes.hurdle <- hurdle(visits ~ . | chronic + school + gender,
                      data = nmes, dist = "negbin")
summary(nmes.hurdle)
exp(coef(nmes.hurdle))

## zero-inflated regression model
nmes.zinb <- zeroinfl(visits ~ . | chronic + school + gender,
                      data = nmes, dist = "negbin")
summary(nmes.zinb)
exp(coef(nmes.zinb))
