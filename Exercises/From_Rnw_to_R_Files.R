f <- "Exercises/Day1_Exercises_Solutions.Rnw"
# Sweave2knitr(f)
knitr::purl(f, documentation = 0)
Stangle(f)
?purl
