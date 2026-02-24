# Cassava Data Analysis - R Script
# Equivalent to the SAS PROC MIXED/GLM analysis with ENTRY as random

# ============================================================================
# STEP 0: INSTALL AND LOAD REQUIRED PACKAGES
# ============================================================================

# List of required packages
packages <- c("readr", "dplyr", "tidyr", "lme4", "lmerTest", "emmeans", 
              "multcomp", "multcompView", "Matrix")

# Install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))

# Set options to match SAS output format
options(digits = 4)
options(width = 132)
options(scipen = 999)  # Avoid scientific notation

# ============================================================================
# STEP 1: IMPORT THE DATA
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 1: Importing Data\n")
cat("============================================================================\n")

# Read the CSV file
data_raw <- read.csv("SAS.csv", header = TRUE, sep = ",", 
                     stringsAsFactors = FALSE, check.names = FALSE)

# Display basic information about the dataset
cat("Dataset Dimensions:", nrow(data_raw), "rows x", ncol(data_raw), "columns\n")
cat("Column names:", paste(names(data_raw), collapse = ", "), "\n\n")

# ============================================================================
# STEP 2: CREATE THE ANALYSIS DATASET
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 2: Creating Analysis Dataset (CAS)\n")
cat("============================================================================\n")

# Use dplyr::select explicitly to avoid conflicts
cas <- data_raw %>%
  dplyr::select(ENV, ENTRY, BLK, REP, FYLD) %>%
  # Convert character columns to factors for mixed modeling
  dplyr::mutate(
    ENV = as.factor(ENV),
    ENTRY = as.factor(ENTRY),
    BLK = as.factor(BLK),
    REP = as.factor(REP),
    FYLD = as.numeric(FYLD)
  ) %>%
  # Remove rows with missing FYLD
  dplyr::filter(!is.na(FYLD))

cat("Analysis Dataset Structure:\n")
str(cas)

cat("\nSummary Statistics for FYLD:\n")
print(summary(cas$FYLD))

cat("\nNumber of observations by ENV:\n")
print(table(cas$ENV))

cat("\nNumber of unique ENTRIES:", length(unique(cas$ENTRY)), "\n")

# ============================================================================
# STEP 3: CREATE INTERACTION TERMS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 3: Creating Interaction Terms\n")
cat("============================================================================\n")

# Create interaction terms for the model
cas$REP_ENV <- interaction(cas$REP, cas$ENV, sep = "_")
cas$BLK_REP_ENV <- interaction(cas$BLK, cas$REP, cas$ENV, sep = "_")
cas$ENTRY_ENV <- interaction(cas$ENTRY, cas$ENV, sep = "_")

cat("Interaction terms created successfully\n")

# ============================================================================
# STEP 4: FIT THE MIXED MODEL WITH ENTRY AS RANDOM
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 4: Fitting Mixed Model with ENTRY as Random Effect\n")
cat("============================================================================\n")

# Check if we have enough data to fit the model
if(nrow(cas) < 10) {
  stop("Insufficient data to fit the model")
}

# Fit the mixed model with ENTRY as random effect
# Model: FYLD = (1|ENV) + (1|ENTRY) + (1|BLK:REP:ENV) + (1|REP:ENV) + (1|ENTRY:ENV)
tryCatch({
  mixed_model <- lmer(FYLD ~ 
                        (1 | ENV) + 
                        (1 | ENTRY) + 
                        (1 | BLK:REP:ENV) + 
                        (1 | REP:ENV) + 
                        (1 | ENTRY:ENV),
                      data = cas,
                      REML = TRUE,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))
  
  cat("✓ Model fitted successfully with ENTRY as random effect\n\n")
  
}, error = function(e) {
  cat("✗ Error fitting full model:", e$message, "\n")
  cat("Attempting to fit simplified model...\n")
  
  # Try simplified model if full model fails
  mixed_model <<- lmer(FYLD ~ 
                         (1 | ENV) + 
                         (1 | ENTRY) + 
                         (1 | REP:ENV) + 
                         (1 | ENTRY:ENV),
                       data = cas,
                       REML = TRUE,
                       control = lmerControl(optimizer = "bobyqa"))
  
  cat("✓ Simplified model fitted successfully with ENTRY as random effect\n")
})

# Display model summary
cat("\nMixed Model Summary:\n")
print(summary(mixed_model))

# ============================================================================
# STEP 5: VARIANCE COMPONENT ESTIMATES (INCLUDING ENTRY)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 5: Variance Component Estimates (Including ENTRY)\n")
cat("============================================================================\n")

# Extract variance components
var_comp <- as.data.frame(VarCorr(mixed_model))

# Calculate total variance and percentages
total_var <- sum(var_comp$vcov, na.rm = TRUE) + (sigma(mixed_model)^2)
var_comp$percent <- (var_comp$vcov / total_var) * 100
residual_var <- sigma(mixed_model)^2
residual_percent <- (residual_var / total_var) * 100

cat("Variance Components Table:\n")
print(var_comp)

cat("\nVariance Components with Percentages:\n")
cat("======================================\n")
for(i in 1:nrow(var_comp)) {
  if(!is.na(var_comp$grp[i])) {
    cat(sprintf("%-25s: Variance = %8.4f (%5.2f%% of total)\n", 
                var_comp$grp[i], var_comp$vcov[i], var_comp$percent[i]))
  }
}
cat(sprintf("%-25s: Variance = %8.4f (%5.2f%% of total)\n", 
            "Residual", residual_var, residual_percent))

# Extract ENTRY variance specifically
entry_var <- var_comp$vcov[var_comp$grp == "ENTRY"]
if(length(entry_var) > 0) {
  cat("\n✓ GENOTYPE (ENTRY) Variance Component:\n")
  cat(sprintf("   ENTRY Variance = %.4f (%.2f%% of total variance)\n", 
              entry_var, (entry_var/total_var)*100))
  cat(sprintf("   ENTRY Standard Deviation = %.4f\n", sqrt(entry_var)))
}

# Calculate heritability (broad-sense)
if(length(entry_var) > 0) {
  # Approximate heritability = Vg / (Vg + Ve/r)
  # where r is the average number of reps per genotype
  avg_reps <- mean(table(cas$ENTRY))
  heritability <- entry_var / (entry_var + residual_var/avg_reps)
  
  cat("\n======================================\n")
  cat("Genetic Parameters:\n")
  cat("======================================\n")
  cat(sprintf("Average replicates per genotype: %.2f\n", avg_reps))
  cat(sprintf("Broad-sense heritability (H²): %.4f (%.2f%%)\n", 
              heritability, heritability*100))
}

# ============================================================================
# STEP 6: TYPE III ANOVA TABLE (for fixed effects - none in this model)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 6: Model Fit Statistics\n")
cat("============================================================================\n")

# Extract model fit statistics
logLik_val <- logLik(mixed_model)
AIC_val <- AIC(mixed_model)
BIC_val <- BIC(mixed_model)

cat("Log-likelihood:", logLik_val, "\n")
cat("AIC:", AIC_val, "\n")
cat("BIC:", BIC_val, "\n")

# ============================================================================
# STEP 7: BEST LINEAR UNBIASED PREDICTIONS (BLUPs) FOR ENTRY
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 7: BLUPs (Best Linear Unbiased Predictions) for ENTRY\n")
cat("============================================================================\n")

# Extract random effects for ENTRY
entry_blups <- ranef(mixed_model)$ENTRY
entry_blups <- data.frame(
  ENTRY = rownames(entry_blups),
  BLUP = round(entry_blups[,1], 4),
  condSD = round(attr(ranef(mixed_model, condVar = TRUE)$ENTRY, "postVar")[1,1,], 4)
)

# Sort by BLUP value
entry_blups <- entry_blups[order(-entry_blups$BLUP), ]

cat("Top 10 ENTRIES by BLUP value:\n")
print(head(entry_blups, 10))

cat("\nBottom 10 ENTRIES by BLUP value:\n")
print(tail(entry_blups, 10))

# ============================================================================
# STEP 8: PREDICTED MEANS FOR ENTRY (Fixed + Random)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 8: Predicted Means for ENTRY (Overall Mean + BLUP)\n")
cat("============================================================================\n")

# Get overall mean
overall_mean <- fixef(mixed_model)[1]  # Intercept

# Calculate predicted values
entry_predicted <- entry_blups
entry_predicted$Predicted_Mean <- overall_mean + entry_predicted$BLUP
entry_predicted <- entry_predicted[order(-entry_predicted$Predicted_Mean), ]

cat("Top 10 ENTRIES by Predicted Mean:\n")
print(head(entry_predicted, 10))

# ============================================================================
# STEP 9: VARIANCE COMPONENT PLOT
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 9: Generating Variance Components Plot\n")
cat("============================================================================\n")

# Create data frame for plotting variance components
vc_plot_data <- data.frame(
  Component = c(var_comp$grp, "Residual"),
  Variance = c(var_comp$vcov, residual_var),
  Percentage = c(var_comp$percent, residual_percent)
)

# Remove any NA components
vc_plot_data <- vc_plot_data[!is.na(vc_plot_data$Component), ]

# Sort by percentage
vc_plot_data <- vc_plot_data[order(-vc_plot_data$Percentage), ]

cat("Variance Components Summary:\n")
print(vc_plot_data)

# Create bar plot
pdf("variance_components.pdf", width = 10, height = 6)
par(mar = c(10, 4, 4, 2))
barplot(vc_plot_data$Percentage, 
        names.arg = vc_plot_data$Component,
        las = 2,  # Vertical labels
        col = c("lightblue", "lightgreen", "lightyellow", "lightpink", "lightgray")[1:nrow(vc_plot_data)],
        main = "Variance Components as Percentage of Total Variance",
        ylab = "Percentage of Total Variance (%)",
        ylim = c(0, max(vc_plot_data$Percentage) * 1.1))
abline(h = 0)
text(x = 1:nrow(vc_plot_data), 
     y = vc_plot_data$Percentage + 2, 
     labels = paste0(round(vc_plot_data$Percentage, 1), "%"))
dev.off()
cat("✓ Variance components plot saved to variance_components.pdf\n")

# ============================================================================
# STEP 10: MODEL DIAGNOSTICS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 10: Model Diagnostics\n")
cat("============================================================================\n")

# Extract residuals
residuals_model <- resid(mixed_model)
fitted_values <- fitted(mixed_model)

# Basic residual statistics
cat("\nResidual Statistics:\n")
print(summary(residuals_model))

# Check for normality
if(length(residuals_model) <= 5000) {
  shapiro_p <- shapiro.test(residuals_model)$p.value
} else {
  shapiro_p <- shapiro.test(sample(residuals_model, 5000))$p.value
}
cat(sprintf("\nShapiro-Wilk normality test p-value: %.6f\n", shapiro_p))

# Create diagnostic plots
cat("\nGenerating diagnostic plots (saved to file: model_diagnostics.pdf)...\n")

pdf("model_diagnostics.pdf", width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Residuals vs Fitted
plot(fitted_values, residuals_model, 
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 0, 0.3), cex = 0.7)
abline(h = 0, col = "red", lwd = 2)
lines(lowess(fitted_values, residuals_model), col = "blue", lwd = 2)

# Q-Q plot
qqnorm(residuals_model, main = "Normal Q-Q Plot", 
       pch = 19, col = rgb(0, 0, 0, 0.3), cex = 0.7)
qqline(residuals_model, col = "red", lwd = 2)

# Histogram of residuals
hist(residuals_model, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightblue", border = "black",
     breaks = 30, freq = FALSE)
curve(dnorm(x, mean = mean(residuals_model), sd = sd(residuals_model)), 
      add = TRUE, col = "red", lwd = 2)

# Scale-Location plot
sqrt_abs_resid <- sqrt(abs(residuals_model))
plot(fitted_values, sqrt_abs_resid,
     main = "Scale-Location Plot",
     xlab = "Fitted Values", ylab = expression(sqrt("|Residuals|")),
     pch = 19, col = rgb(0, 0, 0, 0.3), cex = 0.7)
lines(lowess(fitted_values, sqrt_abs_resid), col = "red", lwd = 2)

dev.off()
cat("✓ Diagnostic plots saved to model_diagnostics.pdf\n")

# ============================================================================
# STEP 11: EXPORT RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 11: Exporting Results\n")
cat("============================================================================\n")

# Export variance components
write.csv(vc_plot_data, "variance_components.csv", row.names = FALSE)
cat("✓ Variance components exported to variance_components.csv\n")

# Export BLUPs
write.csv(entry_blups, "entry_blups.csv", row.names = FALSE)
cat("✓ ENTRY BLUPs exported to entry_blups.csv\n")

# Export predicted means
write.csv(entry_predicted, "entry_predicted_means.csv", row.names = FALSE)
cat("✓ ENTRY predicted means exported to entry_predicted_means.csv\n")

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

