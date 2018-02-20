sex <- factor(sample(c("female", "male"), 100,
                     replace = TRUE, p = c(0.5, 0.5)))
nationality <- factor(sample(c("Ugandian", "Kenian"), 100,
                             replace = TRUE, p = c(0.5, 0.5)))
skin.color <- factor(sample(c("white", "black"), 100,
                            replace = TRUE, p = c(0.5, 0.5)))
#  Combine data frame
dat <- data.frame(sex = sex, nationality = nationality, skin.color = skin.color)
str(dat)

mod0 <- glm(sex ~ nationality,
           family = binomial, 
           data = dat)
summary(mod0)

mod1 <- glm(skin.color ~ nationality,
            family = binomial, 
            data = dat)
summary(mod1)

mod2 <- glm(skin.color.new ~ nationality,
            family = binomial, 
            data = dat)
summary(mod2)
