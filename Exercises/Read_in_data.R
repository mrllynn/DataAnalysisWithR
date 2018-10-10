
rm(list=ls())

# perulung_ems ------------------------------------------------------------

lung <- read.csv("Exercises/data/perulung_ems.csv", sep = ";")
head(lung)
str(lung)
lung$sex <- factor(lung$sex, levels = c(0, 1), labels = c("female", "male"))
lung$respsymptoms <- factor(lung$respsymptoms, levels = c(0, 1), labels = c("no", "yes"))
str(lung)


# sambia ------------------------------------------------------------------

sambia <- read.csv("Exercises/data/sambia.csv", sep = ",")
head(sambia)
str(sambia)
sambia$sex <- factor(sambia$sex, levels = c(0,1), labels = c("female", "male"))
sambia$work <- factor(sambia$work, levels = c(0,1), labels = c("no", "yes"))
str(sambia)

# bacteria ----------------------------------------------------------------

library("MASS")
data(bacteria)
str(bacteria)
head(bacteria)
bacteria$trt.new <- bacteria$trt
levels(bacteria$trt.new) <- c("placebo", "drug", "drug")
levels(bacteria$trt.new) <- c("placebo", "treated")

# ToothGrowth -------------------------------------------------------------

data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
ToothGrowth$dose.fac <- factor(ToothGrowth$dose, levels = c(0.5, 1.0, 2.0),
                               labels = c("low", "med", "high"))

# chickwts ----------------------------------------------------------------

data(chickwts)
str(chickwts)
head(chickwts)

# water -------------------------------------------------------------------

# install.packages("HSAUR3")
library("HSAUR3")
data("water")
str(water)
head(water)

# chain -------------------------------------------------------------------

library("mi")
data("CHAIN")
chain <- CHAIN
str(chain)
head(chain)
