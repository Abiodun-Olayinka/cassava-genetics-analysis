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


