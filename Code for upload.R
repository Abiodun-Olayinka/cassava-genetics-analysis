############################################################
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
# Correlation Plot of Genotypic Correlations
# Data file: data/IKENNE_META.xlsx
############################################################


# Install the necessary packages if not already installed
#install.packages("readxl")
#install.packages("corrplot")

# Load the libraries
library(readxl)
library(corrplot)


data <- read.csv("MOKWA META.csv")


# Check if the matrix is loaded correctly
print(data)

# Extract the numeric part of the matrix (as the first column and row may be labels)
# Converting data to matrix
cor_matrix <- as.matrix(data[, -1])  # Exclude the first column, assuming it's row names

# Set row names (if needed)
rownames(cor_matrix) <- data[[1]]  # Set the first column as row names

# Print the correlation matrix to verify
print(cor_matrix)

# Create the correlation plot using corrplot
# This will display only the lower triangle of the matrix
corrplot(cor_matrix, 
         method = "circle",      # Use circles to represent correlation
         type = "lower",         # Display only the lower triangle
         order = "original",     # Keep the original order of variables
         tl.col = "black",       # Color for text labels
         tl.srt = 45,            # Rotate text labels by 45 degrees
         col = colorRampPalette(c("red", "white", "blue"))(200),  # Color scale from red to blue
         addCoef.col = "black",  # Add correlation coefficient numbers
         number.cex = 0.8,       # Size of the correlation coefficient text
         cl.pos = "r")           # Color legend position (right side)




############################################################
# NDVI Linear Prediction Model
# Data file: data/ONNEMODIFIED.csv
############################################################

rm(list = ls())
# Display current objects in the environment
print(ls())

# Remove all objects from the environment
rm(list = ls())

# Verify that the environment is now empty
print(ls())

library (rms) # for calibration and validation functions
library (calibrate) # for calibration plots
library (caret) # for macDMne learning models
library (ggplot2) # for data visualization
library(boot)      # for bootstrapping


# read your data from a csv file
data <- read.csv ("ONNENDVI.csv")


# check the structure and summary of your data
str (data)
colnames(data)
summary (data)


# plot the relationsDMp between NDVI and each DM at different time points
ggplot (data, aes (x = NDVI3, y = DM)) +
  geom_point () +
  facet_grid (time ~ DM)


# set the seed for reproducibility
set.seed (123)

# remove any rows with missing values
data <- na.omit (data)

# create an index for splitting the data
split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE)


# create an index for splitting the data
#split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE, na.rm = TRUE)

# create an index for splitting the data
#split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE)

# split the data into training (80%) and testing (20%) sets
train_data <- data[split_index, ]
test_data <- data[-split_index, ]


# load the rms package
library (rms)

# build your model using cph or other functions
#model <- cph (Surv (time, status) ~ NDVI3, data = train_data)

# load the rms package
library (rms)

# build your model using lm or other functions
model <- lm (DM ~ NDVI3, data = train_data)

# calibrate your model using bootstrapping
#DM_cal <- calibrate (model, method = "boot", B = 100)


# plot the calibration results
#plot (DM_cal)



# build a linear regression model for plant DM using NDVI as predictor
DM_model <- lm (DM ~ NDVI3, data = train_data)

# check the model summary and diagnostics
summary (DM_model)
#plot (DM_model)

# calibrate the model using bootstrapping
DM_cal <- calibrate (DM_model, method = "boot", B = 100)

# print and plot the calibration results
print (DM_cal)
#plot (DM_cal)


# predict the plant DM using the testing data
DM_pred <- predict (DM_model, newdata = test_data)

# calculate the R-squared value
R2 <- cor (test_data$DM, DM_pred)^2

# calculate the root mean squared error
RMSE <- sqrt (mean ((test_data$DM - DM_pred)^2))

# calculate the mean absolute error
MAE <- mean (abs (test_data$DM - DM_pred))

# print the validation results
cat ("R-squared:", R2, "\n")
cat ("RMSE:", RMSE, "\n")
cat ("MAE:", MAE, "\n")




############################################################
# NDVI Polynomial Prediction Model
# Data file: data/mokwapoly9.xlsx
############################################################

library(readxl)
# Read the data
# Replace the file path with the actual path to your file
data <- read_excel("mokwapoly9.xlsx", sheet = "Sheet1")

# Display basic information about the dataset
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:\n")
print(names(data))

# Check for missing values in key variables
summary(data[, c("FYLD", "NDVI", "PLTHT", "CHL", "STMDI", "ANGBR", "LODG", "NOHAV", "PPSTD")])

# Remove rows with missing values in the variables of interest
# Convert the data to a data frame for easier manipulation
data_df <- as.data.frame(data)

# Select only the columns we need and remove rows with NA in critical variables
model_data <- data_df[, c("FYLD", "NDVI", "PLTHT", "CHL", "ANGBR", "STMDI", "LODG", "NOHAV", "PPSTD")]

# Check for NA values
na_count <- colSums(is.na(model_data))
cat("\nNumber of NA values per column:\n")
print(na_count)

# Remove rows with NA values
model_data <- na.omit(model_data)
cat("\nRows after removing NAs:", nrow(model_data), "\n")

# Create polynomial terms for each predictor
# This implements the formula:
# FYLD = β0 + β11*NDVI + β12*NDVI² + β21*PLTHT + β22*PLTHT² + β31*CHL + β32*CHL² + 
#        β41*STMDI + β42*STMDI² + β51*ANGBR + β52*ANGBR² + β61*LODG + β62*LODG² + 
#        β71*NOHAV + β72*NOHAV² + β81*PPSTD + β82*PPSTD² + ε

# Method 1: Using I() function within lm
poly_model <- lm(FYLD ~ NDVI + I(NDVI^2) + 
                   PLTHT + I(PLTHT^2) + 
                   CHL + I(CHL^2) + 
                   ANGBR + I(ANGBR^2) + 
                   STMDI + I(STMDI^2) + 
                   LODG + I(LODG^2) + 
                   NOHAV + I(NOHAV^2) + 
                   PPSTD + I(PPSTD^2), 
                 data = model_data)

# Method 2: Using poly() function (produces orthogonal polynomials)
# This can help with multicollinearity issues
poly_model_orthogonal <- lm(FYLD ~ poly(NDVI, 2, raw = FALSE) + 
                              poly(PLTHT, 2, raw = FALSE) + 
                              poly(CHL, 2, raw = FALSE) + 
                              poly(ANGBR, 2, raw = FALSE) + 
                              poly(STMDI, 2, raw = FALSE) + 
                              poly(LODG, 2, raw = FALSE) + 
                              poly(NOHAV, 2, raw = FALSE) + 
                              poly(PPSTD, 2, raw = FALSE), 
                            data = model_data)

# Display model summary
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("POLYNOMIAL REGRESSION MODEL SUMMARY (with raw polynomials)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(summary(poly_model))

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("POLYNOMIAL REGRESSION MODEL SUMMARY (with orthogonal polynomials)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(summary(poly_model_orthogonal))

# Extract coefficients
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL COEFFICIENTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
coefficients_df <- as.data.frame(coef(poly_model))
names(coefficients_df) <- "Coefficient"
print(coefficients_df)

# Calculate and display confidence intervals
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CONFIDENCE INTERVALS (95%)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
conf_int <- confint(poly_model, level = 0.95)
print(conf_int)

# ANOVA table
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ANOVA TABLE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(anova(poly_model))

# Model diagnostics
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL DIAGNOSTICS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# R-squared and Adjusted R-squared
cat("R-squared:", summary(poly_model)$r.squared, "\n")
cat("Adjusted R-squared:", summary(poly_model)$adj.r.squared, "\n")
cat("F-statistic:", summary(poly_model)$fstatistic[1], "\n")
cat("p-value (F-test):", pf(summary(poly_model)$fstatistic[1], 
                            summary(poly_model)$fstatistic[2], 
                            summary(poly_model)$fstatistic[3], 
                            lower.tail = FALSE), "\n")

# AIC and BIC
cat("AIC:", AIC(poly_model), "\n")
cat("BIC:", BIC(poly_model), "\n")

# Check for multicollinearity using Variance Inflation Factor (VIF)
# Install car package if needed
if (!require(car)) {
  install.packages("car")
  library(car)
}

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("VARIANCE INFLATION FACTOR (VIF) - check for multicollinearity\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
tryCatch({
  vif_values <- vif(poly_model)
  print(vif_values)
}, error = function(e) {
  cat("Could not calculate VIF due to: ", e$message, "\n")
})

# Create diagnostic plots
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("GENERATING DIAGNOSTIC PLOTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Set up plotting area
par(mfrow = c(2, 2))
plot(poly_model)
par(mfrow = c(1, 1))

# Additional diagnostic plots
# Residuals vs Fitted with smoother
plot(poly_model$fitted.values, poly_model$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
lines(lowess(poly_model$fitted.values, poly_model$residuals), col = "blue")

# Q-Q plot for normality of residuals
qqnorm(poly_model$residuals, main = "Q-Q Plot of Residuals")
qqline(poly_model$residuals, col = "red")

# Shapiro-Wilk test for normality
cat("\nShapiro-Wilk test for normality of residuals:\n")
shapiro_test <- shapiro.test(poly_model$residuals[1:5000])  # Test only first 5000 due to sample size limit
print(shapiro_test)

# Predicted vs Actual values plot
predicted <- predict(poly_model)
plot(model_data$FYLD, predicted, 
     main = "Predicted vs Actual FYLD",
     xlab = "Actual FYLD", ylab = "Predicted FYLD")
abline(0, 1, col = "red", lty = 2)

# Calculate prediction accuracy metrics
rmse <- sqrt(mean((predicted - model_data$FYLD)^2))
mae <- mean(abs(predicted - model_data$FYLD))
cat("\nPrediction Accuracy Metrics:\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("Mean FYLD:", mean(model_data$FYLD), "\n")
cat("RMSE as % of mean:", (rmse/mean(model_data$FYLD))*100, "%\n")

# Create a summary table of important statistics
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL SUMMARY TABLE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")


# Extract coefficients, standard errors, t-values, and p-values
coef_summary <- summary(poly_model)$coefficients
coef_table <- data.frame(
  Estimate = coef_summary[, 1],
  t_value = coef_summary[, 3],
  Significance = ifelse(coef_summary[, 4] < 0.001, "***",
                        ifelse(coef_summary[, 4] < 0.01, "**",
                               ifelse(coef_summary[, 4] < 0.05, "*",
                                      ifelse(coef_summary[, 4] < 0.1, ".", " "))))
)

# Print the coefficient table
print(coef_table)





🧮 SAS Analysis Script

Uses SAS SAS PROC GLM.

Save as:
  
  SAS_scripts/ANOVA_PROC_GLM.sas


Clean version:
  
  /* ANOVA using PROC GLM */
  
  data cas;
input ENV ENTRY BLK REP FYLD;
datalines;
;
run;

proc glm data=cas;
class ENV ENTRY BLK REP;
model FYLD = ENV BLK(REP*ENV) REP(ENV) ENTRY ENTRY*ENV / ss3;
random ENV BLK(REP*ENV) REP(ENV) ENTRY*ENV / test;
lsmeans ENTRY / stderr;
means ENTRY / lsd;
run;
quit;


