getwd()
setwd("~/Dropbox/201710_Makerere/02_Lectures")
# setwd("~/Dropbox/201710_Makerere/03_Exercises")
# f <- "Day5_Exercises_Solutions.Rnw"
f <- "Brucellosis.sim.Rnw"
# Sweave2knitr(f)
knitr::purl(f, documentation = 0)
Stangle(f)
?purl
