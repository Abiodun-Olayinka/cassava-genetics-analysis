# Load the required packages
library(nlme)
library(qmap)
library(rms)
library(ggplot2)

getwd()
# Set your working directory
setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/NDVI Publication")

# Load your data
data <- read.csv("MOKWA ONNE DATA FOR COR ANALYSIS.csv")

# Set seed for reproducibility
set.seed(123)

# Split your data into calibration and validation sets
data$Set <- "cal"
data[sample(nrow(data), nrow(data) * 0.3), "Set"] <- "val"

# Explore your data and create a scatterplot
ggplot(data, aes(x = NDVI9, y = PLTHTIX)) +
  geom_point(aes(color = Set)) +
  facet_wrap(~Stage) +
  labs(x = "NDVI9", y = "Plant PLTHTIX (cm)", title = "NDVI9 vs plant PLTHTIX")

# Define the nonlinear function
nonlinear_function <- PLTHTIX ~ a / (1 + exp(-b * (NDVI9 - c)))

# Specify fixed and random effects
fixed <- nonlinear_function
random <- list(Location = pdDiag(form = ~ c))

# Fit your NLMM with constant variance
model1 <- nlme(fixed = fixed, random = random, data = data[data$Set == "cal", ], weights = varConst())

# Fit your NLMM with variance proportional to NDVI9
model2 <- nlme(fixed = fixed, random = random, data = data[data$Set == "cal", ], weights = varPower(form = ~ NDVI9))

# Compare models using AIC
AIC(model1, model2)

# Select the best model
best_model <- model2

# Plot standardized residuals vs fitted values
plot(fitted(best_model), resid(best_model, type = "p"), xlab = "Fitted values", ylab = "Standardized residuals")
abline(h = 0, lty = 2)

# Generate predictions for the validation data
pred <- predict(best_model, newdata = data[data$Set == "val", ])

# Calculate accuracy measures
cal <- val.prob(pred, data[data$Set == "val", "PLTHTIX"])
rmse <- RMSE(cal$calibrated, data[data$Set == "val", "PLTHTIX"])
mae <- MAE(cal$calibrated, data[data$Set == "val", "PLTHTIX"])
r2 <- R2(cal$calibrated, data[data$Set == "val", "PLTHTIX"])

# Print accuracy measures
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r2, "\n")

# Combine observed and predicted values into a data frame
results <- data.frame(
  PLTHTIX = c(data[data$Set == "cal", "PLTHTIX"], data[data$Set == "val", "PLTHTIX"]),
  Predicted = c(fitted(best_model), cal$calibrated),
  Set = c(rep("cal", nrow(data[data$Set == "cal", ])), rep("val", nrow(data[data$Set == "val", ]))
  )
  
  # Plot observed vs predicted values
  ggplot(results, aes(x = PLTHTIX, y = Predicted)) +
    geom_point(aes(color = Set)) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    labs(x = "Observed plant PLTHTIX (cm)", y = "Predicted plant PLTHTIX (cm)", title = "Observed vs predicted plant PLTHTIX")
  
  