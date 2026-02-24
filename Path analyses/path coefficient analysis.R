# Display current objects in the environment
print(ls())

# Remove all objects from the environment
rm(list = ls())

# Verify that the environment is now empty
print(ls())
# Display current objects in the environment
print(ls())

# Remove all objects from the environment
rm(list = ls())

# Verify that the environment is now empty
print(ls())

# Install and load the necessary packages
#install.packages("plspm")
library(plspm)
#install.packages("semPlot")

library(semPlot)
setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/META-R")


# Load your data or create a sample dataset for demonstration
# Replace this with your own data loading or creation process
data <- read.csv("COMPLETE MOKWA 3RD AUG 2023.csv")
View(data)
colnames(data)
# Load required library
library(lavaan)

# Specify the path model
model <- '
  FYLD ~ HI + STARCH + STMDI9 + ANGBR9 + PLTHTIII + PLTHTVI + PLTHTIX + PLTHTXII + LODG + NDVI3 + NDVI6 + NDVI9 + rtwt + BRNHB9
'

model <- '
  FYLD ~ HI + STARCH + STMDI9 + ANGBR9 + PLTHTIII + PLTHTVI + PLTHTIX + shtwt + DM + DYLD + LODG + NDVI3 + NDVI6 + NDVI9 + rtwt 
'


# Fit the path model
fit <- sem(model, data = data)

# Print model summary
summary(fit, standardized = TRUE)

# Visualize path diagram (optional, requires 'semPlot' package)
# install.packages("semPlot")
# library(semPlot)
semPaths(fit, style = "lisrel", whatLabels = "std", edge.label.cex = 0.8)






# Load the lavaan package
library(lavaan)

# Load your data (assuming it is in a csv file named "data.csv")
#data <- read.csv("data.csv")

# Specify the path model using the lavaan syntax
# The variable on the left of ~ is the outcome and the ones on the right are the predictors
# You can assign names to the paths using :=
# You can also define new parameters using := and existing parameters
model <- "
  # Direct effects of exogenous variables on FYLD
  FYLD ~ a*HI + b*STARCH + c*STMDI9 + d*ANGBR9 + e*PLTHTIII + f*PLTHTVI + g*PLTHTIX + h*PLTHTXII + i*LODG + j*NDVI3 + k*NDVI6 + l*NDVI9 + m*NDVI12 + n*rtwt + o*BRNHB9
  # Indirect effects of exogenous variables on FYLD through endogenous variables
  HI ~ p*STMDI9 + q*ANGBR9 + r*PLTHTIII + s*PLTHTVI + t*PLTHTIX + u*PLTHTXII + v*LODG + w*NDVI3 + x*NDVI6 + y*NDVI9 + z*NDVI12 + aa*rtwt + ab*BRNHB9
  STARCH ~ ac*STMDI9 + ad*ANGBR9 + ae*PLTHTIII + af*PLTHTVI + ag*PLTHTIX + ah*PLTHTXII + ai*LODG + aj*NDVI3 + ak*NDVI6 + al*NDVI9 + am*NDVI12 + an*rtwt + ao*BRNHB9
  # Total effects of exogenous variables on FYLD
  total_HI := a + p*b + q*c + r*d + s*e + t*f + u*g + v*h + w*i + x*j + y*k + z*l + aa*m + ab*n + ac*o
  total_STARCH := b + ac*a + ad*c + ae*d + af*e + ag*f + ah*g + ai*h + aj*i + ak*j + al*k + am*l + an*m + ao*n + ap*o
"

# Fit the path model using the sem() function
fit <- sem(model, data = data)

# Summarize the results
summary(fit)






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
FYLD ~ HI + STARCH + DM + TYLD + DYLD + shtwt + STMDI9 + ANGBR9 + PLTHTIII + PLTHTVI + PLTHTIX + PLTHTXII + LODG + NDVI3 + NDVI6 + NDVI9 + NDVI12 + rtwt + BRNHB9
STARCH ~ DM + DYLD + rtwt + shtwt + HI
'

model <-'
FYLD ~ HI + STARCH + DM + DYLD + shtwt + STMDI9 + ANGBR9 + PLTHTIII  + LODG + NDVI3  + rtwt 
STARCH ~ DYLD + HI + NDVI3
'

model <-'
FYLD ~ RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'
fit <- cfa (model, data = cassava)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')


semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)


model <- '
FYLD ~ RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'

fit <- cfa (model, data = cassava)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit, 'std', layout = 'circle', bold = TRUE, label.cex = 2.0, edge.width = 2)

semPaths(fit, "std", layout = 'tree', edge.label.cex = 2.0, curvePivot = TRUE, bold = TRUE, label.cex = 2.0, edge.width = 2)

# Generate a summary of the SEM model
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# First SEM path diagram in a circular layout
semPaths(fit, 
         what = 'std',            # Standardized solution
         layout = 'circle',       # Circular layout
         bold = TRUE,             # Bold edges
         label.cex = 2.0,         # Size of labels
         edge.width = 2,          # Width of edges
         title = "(D) Path Coefficient Analysis Plot Mokwa Trial",  # Title for the plot
         title.pos = "top")       # Position title at the top

# Second SEM path diagram in a tree layout
semPaths(fit, 
         what = "std",            # Standardized solution
         layout = 'tree',         # Tree layout
         edge.label.cex = 2.0,    # Size of edge labels
         curvePivot = TRUE,       # Curved edges
         bold = TRUE,             # Bold edges
         label.cex = 2.0,         # Size of labels
         edge.width = 2,          # Width of edges
         title = "(D) Path Coefficient Analysis Plot Ikenne Trial",  # Title for the plot
         title.pos = "top")       # Position title at the top


# Generate a summary of the SEM model
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# First SEM path diagram in a circular layout
semPaths(fit, 
         what = 'std',            # Standardized solution
         layout = 'circle',       # Circular layout
         bold = TRUE,             # Bold edges
         label.cex = 2.0,         # Size of labels
         edge.width = 2)          # Width of edges
title(main = "(F) Path Coefficient Analysis Plot Onne Trial", line = 2)  # Title for the circular layout

# Second SEM path diagram in a tree layout
semPaths(fit, 
         what = "std",            # Standardized solution
         layout = 'tree',         # Tree layout
         edge.label.cex = 2.0,    # Size of edge labels
         curvePivot = TRUE,       # Curved edges
         bold = TRUE,             # Bold edges
         label.cex = 2.0,         # Size of labels
         edge.width = 2)          # Width of edges
title(main = "(D) Path Coefficient Analysis Plot Mokwa Trial", line = 2)    # Title for the tree layout

# Adjust plot margins to create space for the title just below the topmost line
par(mar = c(5, 4, 5, 2))  # Increase top margin (third value) to 5 to make space for the title

# Second SEM path diagram in a tree layout
semPaths(fit, 
         what = "std",            # Standardized solution
         layout = 'tree',         # Tree layout
         edge.label.cex = 2.0,    # Size of edge labels
         curvePivot = TRUE,       # Curved edges
         bold = TRUE,             # Bold edges
         label.cex = 2.0,         # Size of labels
         edge.width = 2)          # Width of edges

# Add the title just below the topmost line
title(main = "(F) Path Coefficient Analysis Plot Onne Trial", line = 3)  # Adjust 'line' to position the title just below the topmost line

model <- '
FYLD ~ + RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'

fit <- cfa (model, data = cassava)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit, 'std', layout = 'circle', bold = TRUE, label.cex = 1.2, edge.color = "black", arrow.color = "black", edge.width = 2)

semPaths(fit, "std", layout = 'tree', edge.label.cex = 1.2, curvePivot = TRUE, bold = TRUE, label.cex = 1.2, edge.color = "black", arrow.color = "black", edge.width = 2)


model <- '
FYLD ~  PLTHT3 + RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT3 + PLTHT6 + PLTHT9 + LODG
'

fit <- cfa(model, data = cassava)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fit, 'std', layout = 'circle', label.cex = 1.2, nodeFontSize = 40)





cassava <- read.csv("ONNE.csv")


model <-'
FYLD ~ RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'
fit <- cfa (model, data = cassava)

summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

semPaths(fit, 'std', layout = 'circle')


semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)


ggcorr(cassava[-c(1:12, 23:36, 38:39, 42:45, 47, 49:63, 65:66, 68:69, 71:72, 74:79 )], nbreaks = 6, label = T, low = "red3", high = "green3", 
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))

library(ggcorrplot)


library(ggplot2)

library(GGally)

To remove any missing values or extreme outliers from your data before fitting the confirmatory factor analysis (CFA) model, you can preprocess the data. Below is your script rewritten with data preprocessing steps included:
  
  ```r
# Read the data from the CSV file
cassava <- read.csv("ONNEMODIFIED.csv")

# Remove missing values and extreme outliers
# You can replace NA with appropriate imputation method or remove them altogether
# You can also apply outlier detection and removal techniques such as winsorization or trimming
cassava_clean <- na.omit(cassava)  # Remove rows with missing values
# Example outlier removal using z-score
cassava_clean <- cassava_clean[apply(cassava_clean, 2, function(x) abs(scale(x)) < 3), ]

# Define the model
model <- '
FYLD ~ PLTHT3 + RTWT + NOHAV + RTNO + HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~ HI + PLTHT3 + PLTHT6 + PLTHT9 + LODG
'

# Fit the CFA model with cleaned data
fit <- cfa(model, data = cassava_clean)

# Summarize the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Plot the structural equation model paths
semPaths(fit, "std", layout = "circle")
semPaths(fit, "std", layout = "tree", edge.label.cex = 0.9, curvePivot = TRUE)
```


The error message suggests that the function `scale()` is expecting numeric data, but it's receiving non-numeric data. This usually occurs when you have non-numeric columns in your dataset. To fix this error, you need to ensure that all columns in your dataset are numeric or convert them to numeric if possible.

Here's a modified version of the code to handle non-numeric columns:
  
  ```r
# Remove missing values and extreme outliers
cassava_clean <- na.omit(cassava)  # Remove rows with missing values

# Exclude non-numeric columns before outlier detection
numeric_cols <- sapply(cassava_clean, is.numeric)
cassava_numeric <- cassava_clean[, numeric_cols]

# Example outlier removal using z-score
cassava_clean <- cassava_numeric[apply(cassava_numeric, 2, function(x) {
  if (is.numeric(x)) {
    all(abs(scale(x)) < 3)
  } else {
    rep(TRUE, length(x))
  }
}), ]

# Define the model
model <- '
FYLD ~ PLTHT3 + RTWT + NOHAV + RTNO + HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~ HI + PLTHT3 + PLTHT6 + PLTHT9 + LODG
'

# Fit the CFA model with cleaned data
fit <- cfa(model, data = cassava_clean)

# Summarize the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Plot the structural equation model paths
semPaths(fit, "std", layout = "circle")
semPaths(fit, "std", layout = "tree", edge.label.cex = 0.9, curvePivot = TRUE)
```


