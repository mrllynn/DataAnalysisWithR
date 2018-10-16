rm(list=ls())

n <- 1000
mean.x <- 50
sd.x <- 40

x <- rnorm(n, mean = mean.x, sd = sd.x)

plot(density(x))
abline(v = mean.x, col = "red", lwd = 2)
abline(v = mean.x + sd.x, col = "blue", lwd = 2, lty = 2)
abline(v = mean.x - sd.x, col = "blue", lwd = 2, lty = 2)


rm(list=ls())

n <- 1000
lambda.x <- 300

x <- rpois(n, lambda = lambda.x)

plot(density(x))
abline(v = lambda.x, col = "red", lwd = 2)


breeding <- rbinom(10, size = 1, prob = 0.5)
table(breeding)

