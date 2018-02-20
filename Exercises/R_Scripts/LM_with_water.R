rm(list=ls())

library("HSAUR3")
data(water)
str(water)

# mod.hard <- lm(mortality ~ hardness, data = water)
mod.hard <- lm(water$mortality ~ water$hardness)
summary(mod.hard)

par(mfrow=c(2,2))
plot(mod.hard)

mod.loc <- lm(mortality ~ location, data = water)
summary(mod.loc)
coef(mod.loc)
levels(water$location)

par(mfrow=c(2,2))
plot(mod.loc)

t.test(water$mortality ~ water$location)

mod.hard.loc <- lm(mortality ~ hardness + location, data = water)
summary(mod.hard.loc)

mod.hard <- lm(mortality ~ hardness, data = water)                # mod 1
mod.loc <- lm(mortality ~ location, data = water)                 # mod 2 
mod.hard.loc <- lm(mortality ~ hardness + location, data = water) # mod 3

mod.hard <- lm(mortality ~ hardness, data = water)                # mod 1
mod.loc <- lm(mortality ~ location, data = water)                 # mod 2 
mod.hard.loc <- lm(mortality ~ hardness + location, data = water) # mod 3
AIC(mod.hard, mod.loc, mod.hard.loc)
round(AIC(mod.hard, mod.loc, mod.hard.loc), 2)

mod.hard.loc <- lm(mortality ~ hardness + location, data = water)
par(mfrow=c(2,2))
plot(mod.hard.loc)
