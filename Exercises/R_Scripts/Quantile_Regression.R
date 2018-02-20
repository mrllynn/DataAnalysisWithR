
library("quantreg")
head(sambia)

### tau = 2.5 %
rq.025 <- rq(zscore ~ sex + age.child + work + bmi + age.mother.birth, 
             tau = 0.025,
             data = sambia)
# summary(rq.025)

### tau = 5 %
rq.05 <- rq(zscore ~ sex + age.child + work + bmi + age.mother.birth, 
            tau = 0.05, 
            data = sambia)
# summary(rq.05)

### tau = 50 %
rq.5 <- rq(zscore ~ sex + age.child + work + bmi + age.mother.birth, 
           tau = 0.5, 
           data = sambia)
# summary(rq.5)


### Comparison
rq_coef <- cbind(coef(rq.025), coef(rq.05), coef(rq.5))
rownames(rq_coef) <- rownames(summary(rq.5)$coefficients)
colnames(rq_coef) <- paste0("tau = ", c("0.025", "0.05", "0.5"))
round(rq_coef, 3)

### Estimate for a grid of quantiles
rq <- rq(zscore ~ sex + age.child + work + bmi + age.mother.birth, 
         tau = seq(from = 0.025, to = 0.5, by = 0.025), 
         data = sambia)

### Plot results
# lm(zscore ~ sex + age.child + work + bmi + age.mother.birth, data = sambia)
plot(rq) # plot including line for the OLS coefficient (as estimated by lm)

# A sequence of coefficient estimates for quantile regressions with
# varying tau parameters is visualized along with associated confidence bands.
plot(summary(rq)) 
