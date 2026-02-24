# Path Coefficient Analysis using SEM
# Requires: R, lavaan, semPlot, OpenMx, tidyverse
# Data file: data/MOKWA.csv
############################################################

library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)

# Organizing package information for table
packages <- c("tidyverse", "knitr", "kableExtra", "lavaan", "semPlot", "OpenMx", "GGally")
display <- c("Package","Title", "Maintainer", "Version", "URL")
table <- matrix(NA, 1, NROW(display), dimnames = list(1, display))
for(i in 1:NROW(packages)){
  list <- packageDescription(packages[i])
  table <- rbind(table, matrix(unlist(list[c(display)]), 1, NROW(display), byrow = T))
}
table[,NROW(display)] <- stringr::str_extract(table[,NROW(display)], ".+,")

# Table of packages
kable(table[-1,], format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


cassava <- read.csv("MOKWA.csv")
colnames(cassava)
view(cassava)

model <-'
FYLD ~ RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'
fit <- cfa (model, data = cassava)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')


semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)





############################################################
