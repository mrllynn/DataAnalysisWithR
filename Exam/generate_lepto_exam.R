
rm(list=ls())

set.seed(2018)

n <- 200

gender <- factor(rbinom(n, size = 1, prob = 0.5),
                 levels = c(0, 1),
                 labels = c("female", "male"))
gender

age <- rpois(n, lambda = 35)
age
boxplot(age)

exposure <- factor(rep(c("urban", "rural"), each = n / 2),
                   labels = c("urban", "rural"))
exposure

antibodies <- factor(c(rbinom(n/2, size = 1, prob = 0.8),
                       rbinom(n/2, size = 1, prob = 0.3)),
                     levels = c(0, 1),
                     labels = c("absent", "presence"))
antibodies
table(exposure, antibodies)

num.rats <- c(rpois(n/2, lambda = 5),
              rpois(n/2, lambda = 0.5))
num.rats
boxplot(num.rats ~ exposure)

lepto_exam <- data.frame(gender, age, exposure, antibodies, num.rats)

# head(lepto_exam, 20)
# str(lepto_exam)
# summary(lepto_exam)

write.csv(lepto_exam, file = "lepto_exam.csv")

