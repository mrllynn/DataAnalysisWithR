# install.packages("mi")
library("mi")
data("CHAIN")
?CHAIN
chain <- CHAIN # rename the data frame
names(chain)
str(chain)
summary(chain)

# chain$income <- factor(chain$income,
#                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#                        labels = c("1", "2", "3", "4", "5",
#                                   "6", "7","8", "9", "10"))
# chain$income <- factor(chain$income,
#                        levels = c("1", "2", "3", "4", "5",
#                                   "6", "7","8", "9", "10"))

chain$income <- factor(chain$income, levels = c(1:10),
                       labels = c("1", "2", "3", "4", "5",
                                  "6", "7","8", "9", "10"))
chain$mental <- factor(chain$mental, levels = c(0, 1),
                       labels = c("no", "yes"))
chain$damage <- factor(chain$damage, levels = c(1:5),
                       labels = c("1", "2", "3", "4", "5"))
chain$treatment <- factor(chain$treatment, levels = c(0:2),
                          labels = c("0", "1", "2"))

str(chain)
summary(chain)

par(mfrow=c(1,1))
hist(chain$log_virus, ylim = c(0, 200), col = "darkgray")
nrow(chain) # [1] 532
apply(chain, 2, function(x) {sum(is.na(x))})

# Complete Case Analysis --------------------------------------------------

lm.mod.NA <- lm(log_virus ~ age + income + healthy + mental +
                  damage + treatment, data = chain)
summary(lm.mod.NA)

par(mfrow=c(2,2))
plot(lm.mod.NA)

dim(chain) # [1] 532   7
apply(chain, 2, function(x){sum(is.na(x))})
# log_virus       age    income   healthy    mental    damage treatment 
#       179        24        38        24        24        63        24 

# missForest Analysis -----------------------------------------------------

# install.packages("missForest")
library("missForest")
# Nonparametric Missing Value Imputation using Random Forest
chain.imp <- missForest(chain, verbose = FALSE, variablewise = TRUE)
chain.imp$OOBerror # estimated imputation error for each variable
chain.final.rf <- chain.imp$ximp
dim(chain.final.rf) # [1] 532   7
apply(chain.final.rf, 2, function(x){sum(is.na(x))})
# log_virus       age    income   healthy    mental    damage treatment 
#         0         0         0         0         0         0         0 
lm.mod.rf <- lm(log_virus ~ age + income + healthy +
                  mental + damage + treatment,
                data = chain.final.rf)

# mice Analysis -----------------------------------------------------------

# install.packages("mice")
library("mice")
# Multivariate Imputation by Chained Equations (MICE)
# create 4 imputed data sets via mice

# IMPUTE DATA
chain.mice <- mice(chain, m = 4, print = FALSE, seed = 2017)

# ANALYSE RESULTS WITH IMPUTED DATA
# fit for each of the 4 data sets a linear regression
fit.4.mice <- with(chain.mice, lm(log_virus ~ age + income + healthy +
                                    mental + damage + treatment))
# check out the regression coefficients of the 4 fits
summary(fit.4.mice)

# POOL ANALYSIS RESULTS
# pool the results of the 4 models with Rubin's rule
lm.mod.mice <- pool(fit.4.mice)

# # pool(...)
# # calling a normal function within a package
# mice::pool(...)
# mi::pool(...)

# # install.packages("mi")
# library("mi")
# 
# # install.packages("missForest")
# library("missForest")
# 
# # install.packages("mice")
# library("mice")



# combined regression coefficients of the 4 fits
summary(lm.mod.mice)

round(summary(lm.mod.NA)$coef, 2)
round(summary(lm.mod.rf)$coef, 2)

round(summary(lm.mod.NA)$coef, 2)
round(summary(lm.mod.mice)[,c(1,2,3,5)], 2)
