# Cassava Data Analysis - R Script
# Equivalent to the SAS PROC MIXED/GLM analysis

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

# Preview first few rows
cat("First 6 rows of data:\n")
print(head(data_raw, 6))

# ============================================================================
# STEP 2: CREATE THE ANALYSIS DATASET (equivalent to SAS DATA step)
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
  # Remove rows with missing FYLD (as SAS would do)
  dplyr::filter(!is.na(FYLD))

# Print the structure of the dataset
cat("Analysis Dataset Structure:\n")
str(cas)

# Display summary statistics
cat("\nSummary Statistics for FYLD:\n")
print(summary(cas$FYLD))

cat("\nNumber of observations by ENV:\n")
print(table(cas$ENV))

cat("\nNumber of observations by ENTRY (first 20):\n")
entry_counts <- sort(table(cas$ENTRY), decreasing = TRUE)
print(head(entry_counts, 20))

# ============================================================================
# STEP 3: PROC PRINT EQUIVALENT
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 3: PROC PRINT - First 20 observations of CAS dataset:\n")
cat("============================================================================\n")
print(head(cas, 20))

# ============================================================================
# STEP 4: CREATE INTERACTION TERMS FOR THE MODEL
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 4: Creating Interaction Terms\n")
cat("============================================================================\n")

# Create interaction terms for the model
cas$REP_ENV <- interaction(cas$REP, cas$ENV, sep = "_")
cas$BLK_REP_ENV <- interaction(cas$BLK, cas$REP, cas$ENV, sep = "_")
cas$ENTRY_ENV <- interaction(cas$ENTRY, cas$ENV, sep = "_")

cat("Interaction terms created successfully\n")
cat("Number of unique REP_ENV combinations:", length(unique(cas$REP_ENV)), "\n")
cat("Number of unique BLK_REP_ENV combinations:", length(unique(cas$BLK_REP_ENV)), "\n")
cat("Number of unique ENTRY_ENV combinations:", length(unique(cas$ENTRY_ENV)), "\n")

# ============================================================================
# STEP 5: FIT THE MIXED MODEL
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 5: Fitting Mixed Model with lmer\n")
cat("============================================================================\n")

# Check if we have enough data to fit the model
if(nrow(cas) < 10) {
  stop("Insufficient data to fit the model")
}

# Fit the mixed model
# Model: FYLD = ENTRY + (1|ENV) + (1|BLK:REP:ENV) + (1|REP:ENV) + (1|ENTRY:ENV)
tryCatch({
  mixed_model <- lmer(FYLD ~ ENTRY + 
                        (1 | ENV) + 
                        (1 | BLK:REP:ENV) + 
                        (1 | REP:ENV) + 
                        (1 | ENTRY:ENV),
                      data = cas,
                      REML = TRUE,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))
  
  cat("✓ Model fitted successfully\n\n")
  
}, error = function(e) {
  cat("✗ Error fitting full model:", e$message, "\n")
  cat("Attempting to fit simplified model...\n")
  
  # Try simplified model if full model fails
  mixed_model <<- lmer(FYLD ~ ENTRY + 
                         (1 | ENV) + 
                         (1 | REP:ENV) + 
                         (1 | ENTRY:ENV),
                       data = cas,
                       REML = TRUE,
                       control = lmerControl(optimizer = "bobyqa"))
  
  cat("✓ Simplified model fitted successfully\n")
})

# Display model summary
cat("\nMixed Model Summary:\n")
print(summary(mixed_model))

# ============================================================================
# STEP 6: TYPE III ANOVA TABLE
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 6: Type III Analysis of Variance\n")
cat("============================================================================\n")

# Get Type III ANOVA table
tryCatch({
  anova_table <- anova(mixed_model, type = 3, ddf = "Kenward-Roger")
  print(anova_table)
}, error = function(e) {
  cat("Note: Kenward-Roger approximation failed, using Satterthwaite instead\n")
  anova_table <<- anova(mixed_model, type = 3, ddf = "Satterthwaite")
  print(anova_table)
})

# ============================================================================
# STEP 7: VARIANCE COMPONENT ESTIMATES
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 7: Variance Component Estimates\n")
cat("============================================================================\n")

# Extract variance components
var_comp <- as.data.frame(VarCorr(mixed_model))

# Calculate total variance and percentages
total_var <- sum(var_comp$vcov, na.rm = TRUE)
var_comp$percent <- (var_comp$vcov / total_var) * 100

cat("Variance Components:\n")
print(var_comp)

cat("\nVariance Components with Percentages:\n")
for(i in 1:nrow(var_comp)) {
  if(!is.na(var_comp$grp[i])) {
    cat(sprintf("%-20s: %8.4f (%5.2f%% of total)\n", 
                var_comp$grp[i], var_comp$vcov[i], var_comp$percent[i]))
  }
}
cat(sprintf("%-20s: %8.4f\n", "Residual", sigma(mixed_model)^2))

# ============================================================================
# STEP 8: LSMEANS FOR ENTRY
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 8: LSMEANS for ENTRY\n")
cat("============================================================================\n")

# Calculate LSMEANS for ENTRY
lsmeans_entry <- emmeans(mixed_model, ~ ENTRY, 
                         lmer.df = "kenward-roger",
                         adjust = "none")

# Display LSMEANS
print(lsmeans_entry)

# ============================================================================
# STEP 9: LSMEANS WITH STANDARD ERRORS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 9: LSMEANS for ENTRY with Standard Errors\n")
cat("============================================================================\n")

# Extract LSMEANS with standard errors and confidence intervals
lsmeans_df <- as.data.frame(summary(lsmeans_entry))
print(lsmeans_df)

# Sort by LSMEAN value
lsmeans_sorted <- lsmeans_df[order(-lsmeans_df$emmean), ]
cat("\nTop 10 ENTRIES by LSMEAN (highest to lowest):\n")
print(head(lsmeans_sorted, 10))

# ============================================================================
# STEP 10: OBSERVED MEANS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 10: Observed Means by ENTRY\n")
cat("============================================================================\n")

# Calculate observed means
observed_means <- cas %>%
  dplyr::group_by(ENTRY) %>%
  dplyr::summarise(
    N = n(),
    Mean = mean(FYLD, na.rm = TRUE),
    SD = sd(FYLD, na.rm = TRUE),
    SE = SD / sqrt(N),
    Min = min(FYLD),
    Max = max(FYLD),
    .groups = 'drop'
  ) %>%
  dplyr::arrange(desc(Mean))

cat("Observed Means by ENTRY (first 20):\n")
print(as.data.frame(head(observed_means, 20)), digits = 4)

# ============================================================================
# STEP 11: LSD ANALYSIS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 11: LSD (Least Significant Difference) Analysis\n")
cat("============================================================================\n")

# Get pairwise comparisons
pairwise_comparisons <- pairs(lsmeans_entry, adjust = "none")
pairwise_summary <- as.data.frame(summary(pairwise_comparisons))

# Extract residual variance (MSE)
residual_var <- sigma(mixed_model)^2
residual_sd <- sigma(mixed_model)

# Approximate LSD using average degrees of freedom
# Get error df from the ANOVA table
if(exists("anova_table")) {
  df_error <- ifelse("DenDF" %in% names(anova_table), 
                     anova_table$DenDF[anova_table$Term == "ENTRY"],
                     anova_table$df[anova_table$Term == "ENTRY"])
} else {
  df_error <- 100  # Fallback value
}

t_critical <- qt(0.975, df = df_error)  # two-tailed at alpha=0.05

# Calculate average LSD based on harmonic mean of sample sizes
n_entries <- table(cas$ENTRY)
harm_mean_n <- 1 / mean(1 / n_entries[n_entries > 0])
approx_lsd <- t_critical * sqrt(2 * residual_var / harm_mean_n)

cat(sprintf("Residual Variance (MSE): %.4f\n", residual_var))
cat(sprintf("Residual SD: %.4f\n", residual_sd))
cat(sprintf("Error DF: %.1f\n", df_error))
cat(sprintf("t-critical (0.05, two-tailed): %.4f\n", t_critical))
cat(sprintf("Harmonic mean of n per ENTRY: %.2f\n", harm_mean_n))
cat(sprintf("Approximate LSD (0.05): %.4f\n", approx_lsd))

cat("\nPairwise Comparisons Summary (first 20 rows):\n")
print(head(pairwise_summary, 20))

# Create compact letter display for significant differences
tryCatch({
  cld_results <- cld(lsmeans_entry, 
                     adjust = "none",
                     alpha = 0.05,
                     Letters = letters)
  
  cat("\nCompact Letter Display (LSD, α=0.05):\n")
  print(cld_results)
}, error = function(e) {
  cat("Note: Could not create compact letter display:", e$message, "\n")
})

# ============================================================================
# STEP 12: MODEL DIAGNOSTICS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 12: Model Diagnostics\n")
cat("============================================================================\n")

# Extract residuals
residuals_model <- resid(mixed_model)
fitted_values <- fitted(mixed_model)

# Basic residual statistics
cat("\nResidual Statistics:\n")
print(summary(residuals_model))

# Check for normality (limit to 5000 observations for Shapiro test)
if(length(residuals_model) <= 5000) {
  shapiro_p <- shapiro.test(residuals_model)$p.value
} else {
  shapiro_p <- shapiro.test(sample(residuals_model, 5000))$p.value
}
cat(sprintf("\nShapiro-Wilk normality test p-value: %.6f\n", shapiro_p))
if(shapiro_p > 0.05) {
  cat("✓ Residuals appear normally distributed (p > 0.05)\n")
} else {
  cat("✗ Residuals deviate from normality (p < 0.05)\n")
}

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
# STEP 13: SUMMARY OF KEY RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("SUMMARY OF KEY RESULTS\n")
cat("============================================================================\n")

cat("\nTop 10 ENTRIES by LSMEAN:\n")
top10 <- head(lsmeans_sorted, 10)
print(top10[, c("ENTRY", "emmean", "SE", "df")])

cat("\nBottom 10 ENTRIES by LSMEAN:\n")
bottom10 <- tail(lsmeans_sorted, 10)
print(bottom10[, c("ENTRY", "emmean", "SE", "df")])

cat("\nVariance Components Summary:\n")
for(i in 1:nrow(var_comp)) {
  if(!is.na(var_comp$grp[i])) {
    cat(sprintf("  %-20s: Variance = %.4f (%.1f%%)\n", 
                var_comp$grp[i], var_comp$vcov[i], var_comp$percent[i]))
  }
}
cat(sprintf("  %-20s: Variance = %.4f\n", "Residual", sigma(mixed_model)^2))

# ============================================================================
# STEP 14: EXPORT RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 14: Exporting Results\n")
cat("============================================================================\n")

# Export LSMEANS
write.csv(lsmeans_df, "lsmeans_entry.csv", row.names = FALSE)
cat("✓ LSMEANS exported to lsmeans_entry.csv\n")

# Export variance components
write.csv(var_comp, "variance_components.csv", row.names = FALSE)
cat("✓ Variance components exported to variance_components.csv\n")

# Export observed means
write.csv(observed_means, "observed_means.csv", row.names = FALSE)
cat("✓ Observed means exported to observed_means.csv\n")

# Export ANOVA table
if(exists("anova_table")) {
  write.csv(as.data.frame(anova_table), "anova_table.csv", row.names = FALSE)
  cat("✓ ANOVA table exported to anova_table.csv\n")
}

# Export pairwise comparisons
write.csv(pairwise_summary, "pairwise_comparisons.csv", row.names = FALSE)
cat("✓ Pairwise comparisons exported to pairwise_comparisons.csv\n")

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")