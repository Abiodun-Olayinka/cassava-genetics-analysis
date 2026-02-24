# Cassava Data Analysis - R Script
# Mixed Model Analysis with ANOVA-style Output

# ============================================================================
# STEP 0: INSTALL AND LOAD REQUIRED PACKAGES
# ============================================================================

# List of required packages
packages <- c("readr", "dplyr", "tidyr", "lme4", "lmerTest", "emmeans", 
              "multcomp", "multcompView", "Matrix", "car", "pbkrtest")

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
# STEP 4: FIT TWO MODELS - ONE WITH ENTRY AS FIXED (for ANOVA table)
# AND ONE WITH ENTRY AS RANDOM (for variance components)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 4: Fitting Mixed Models\n")
cat("============================================================================\n")

# Check if we have enough data to fit the model
if(nrow(cas) < 10) {
  stop("Insufficient data to fit the model")
}

# ============================================
# Model 1: ENTRY as FIXED effect (for ANOVA table)
# ============================================
cat("\n--- Model 1: ENTRY as FIXED effect (for ANOVA table) ---\n")

tryCatch({
  model_fixed <- lmer(FYLD ~ ENTRY + 
                        (1 | ENV) + 
                        (1 | BLK:REP:ENV) + 
                        (1 | REP:ENV) + 
                        (1 | ENTRY:ENV),
                      data = cas,
                      REML = TRUE,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))
  
  cat("✓ Model with ENTRY as fixed effect fitted successfully\n\n")
  
}, error = function(e) {
  cat("✗ Error fitting fixed effects model:", e$message, "\n")
  cat("Attempting to fit simplified model...\n")
  
  model_fixed <<- lmer(FYLD ~ ENTRY + 
                         (1 | ENV) + 
                         (1 | REP:ENV) + 
                         (1 | ENTRY:ENV),
                       data = cas,
                       REML = TRUE,
                       control = lmerControl(optimizer = "bobyqa"))
  
  cat("✓ Simplified fixed effects model fitted successfully\n")
})

# ============================================
# Model 2: ENTRY as RANDOM effect (for variance components)
# ============================================
cat("\n--- Model 2: ENTRY as RANDOM effect (for variance components) ---\n")

tryCatch({
  model_random <- lmer(FYLD ~ 
                         (1 | ENV) + 
                         (1 | ENTRY) + 
                         (1 | BLK:REP:ENV) + 
                         (1 | REP:ENV) + 
                         (1 | ENTRY:ENV),
                       data = cas,
                       REML = TRUE,
                       control = lmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 100000)))
  
  cat("✓ Model with ENTRY as random effect fitted successfully\n\n")
  
}, error = function(e) {
  cat("✗ Error fitting random effects model:", e$message, "\n")
  cat("Attempting to fit simplified model...\n")
  
  model_random <<- lmer(FYLD ~ 
                          (1 | ENV) + 
                          (1 | ENTRY) + 
                          (1 | REP:ENV) + 
                          (1 | ENTRY:ENV),
                        data = cas,
                        REML = TRUE,
                        control = lmerControl(optimizer = "bobyqa"))
  
  cat("✓ Simplified random effects model fitted successfully\n")
})

# ============================================================================
# STEP 5: ANOVA TABLE FOR FIXED EFFECTS (with Df, Sum Sq, Mean Sq, F value, Pr(>F))
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 5: ANOVA Table for Fixed Effects\n")
cat("============================================================================\n")
cat("                    Df   Sum Sq   Mean Sq   F value    Pr(>F)\n")
cat("----------------------------------------------------------------------------\n")

# Method 1: Using anova() from lmerTest
tryCatch({
  anova_fixed <- anova(model_fixed, type = 3, ddf = "Kenward-Roger")
  
  # Format and display the ANOVA table
  anova_table <- data.frame(
    Effect = rownames(anova_fixed),
    Df = anova_fixed$NumDF,
    DenDF = round(anova_fixed$DenDF, 1),
    `Sum Sq` = round(anova_fixed$`Sum Sq`, 4),
    `Mean Sq` = round(anova_fixed$`Mean Sq`, 4),
    `F value` = round(anova_fixed$`F value`, 4),
    `Pr(>F)` = format.pval(anova_fixed$`Pr(>F)`, digits = 4, eps = 0.0001)
  )
  
  print(anova_table, row.names = FALSE)
  
}, error = function(e) {
  cat("Note: Kenward-Roger approximation failed, using Satterthwaite instead\n")
  
  anova_fixed <- anova(model_fixed, type = 3, ddf = "Satterthwaite")
  
  anova_table <- data.frame(
    Effect = rownames(anova_fixed),
    Df = anova_fixed$NumDF,
    DenDF = round(anova_fixed$DenDF, 1),
    `Sum Sq` = round(anova_fixed$`Sum Sq`, 4),
    `Mean Sq` = round(anova_fixed$`Mean Sq`, 4),
    `F value` = round(anova_fixed$`F value`, 4),
    `Pr(>F)` = format.pval(anova_fixed$`Pr(>F)`, digits = 4, eps = 0.0001)
  )
  
  print(anova_table, row.names = FALSE)
})

# ============================================================================
# STEP 6: TYPE III ANOVA USING CAR PACKAGE (Alternative method)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 6: Type III ANOVA using car::Anova\n")
cat("============================================================================\n")

tryCatch({
  # This requires fitting the model with REML = FALSE
  model_fixed_ml <- lmer(FYLD ~ ENTRY + 
                           (1 | ENV) + 
                           (1 | BLK:REP:ENV) + 
                           (1 | REP:ENV) + 
                           (1 | ENTRY:ENV),
                         data = cas,
                         REML = FALSE)
  
  anova_car <- Anova(model_fixed_ml, type = "III", test.statistic = "F")
  
  car_table <- data.frame(
    Effect = rownames(anova_car),
    Df = anova_car$Df,
    `F value` = round(anova_car$F, 4),
    `Pr(>F)` = format.pval(anova_car$`Pr(>F)`, digits = 4, eps = 0.0001)
  )
  
  print(car_table, row.names = FALSE)
  
}, error = function(e) {
  cat("Could not compute car::Anova:", e$message, "\n")
})

# ============================================================================
# STEP 7: VARIANCE COMPONENT ESTIMATES (from random effects model)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 7: Variance Component Estimates (Random Effects Model)\n")
cat("============================================================================\n")

# Extract variance components
var_comp <- as.data.frame(VarCorr(model_random))

# Calculate total variance and percentages
total_var <- sum(var_comp$vcov, na.rm = TRUE) + (sigma(model_random)^2)
var_comp$percent <- (var_comp$vcov / total_var) * 100
residual_var <- sigma(model_random)^2
residual_percent <- (residual_var / total_var) * 100

cat("\nVariance Components Table:\n")
cat("======================================\n")
cat(sprintf("%-25s %12s %12s\n", "Component", "Variance", "% of Total"))
cat("--------------------------------------\n")
for(i in 1:nrow(var_comp)) {
  if(!is.na(var_comp$grp[i])) {
    cat(sprintf("%-25s %12.4f %11.2f%%\n", 
                var_comp$grp[i], var_comp$vcov[i], var_comp$percent[i]))
  }
}
cat(sprintf("%-25s %12.4f %11.2f%%\n", "Residual", residual_var, residual_percent))
cat("======================================\n")

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
# STEP 8: LIKELIHOOD RATIO TESTS FOR RANDOM EFFECTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 8: Likelihood Ratio Tests for Random Effects\n")
cat("============================================================================\n")

# Function to perform LRT for a random effect
test_random_effect <- function(full_model, reduced_formula, effect_name) {
  tryCatch({
    reduced_model <- lmer(reduced_formula, data = cas, REML = FALSE)
    lrt <- anova(reduced_model, full_model, refit = FALSE)
    
    cat(sprintf("\nTesting %s:\n", effect_name))
    cat(sprintf("  Chi-sq = %.4f, Df = %d, p-value = %.6f\n", 
                lrt$Chisq[2], lrt$Df[2], lrt$`Pr(>Chisq)`[2]))
  }, error = function(e) {
    cat(sprintf("  Could not test %s: %s\n", effect_name, e$message))
  })
}

# Fit full model with ML for LRT
full_ml <- lmer(FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | REP:ENV) + (1 | ENTRY:ENV),
                data = cas, REML = FALSE)

# Test each random effect
test_random_effect(full_ml, "FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | ENTRY:ENV)", "REP:ENV")
test_random_effect(full_ml, "FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | REP:ENV)", "ENTRY:ENV")
test_random_effect(full_ml, "FYLD ~ (1 | ENV) + (1 | REP:ENV) + (1 | ENTRY:ENV)", "ENTRY")
test_random_effect(full_ml, "FYLD ~ (1 | ENTRY) + (1 | REP:ENV) + (1 | ENTRY:ENV)", "ENV")

# ============================================================================
# STEP 9: BEST LINEAR UNBIASED PREDICTIONS (BLUPs) FOR ENTRY
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 9: BLUPs (Best Linear Unbiased Predictions) for ENTRY\n")
cat("============================================================================\n")

# Extract random effects for ENTRY
entry_blups <- ranef(model_random)$ENTRY
entry_blups <- data.frame(
  ENTRY = rownames(entry_blups),
  BLUP = round(entry_blups[,1], 4),
  condSD = round(attr(ranef(model_random, condVar = TRUE)$ENTRY, "postVar")[1,1,], 4)
)

# Sort by BLUP value
entry_blups <- entry_blups[order(-entry_blups$BLUP), ]

cat("Top 10 ENTRIES by BLUP value:\n")
print(head(entry_blups, 10))

cat("\nBottom 10 ENTRIES by BLUP value:\n")
print(tail(entry_blups, 10))

# ============================================================================
# STEP 10: PREDICTED MEANS FOR ENTRY
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 10: Predicted Means for ENTRY (Overall Mean + BLUP)\n")
cat("============================================================================\n")

# Get overall mean
overall_mean <- fixef(model_random)[1]  # Intercept

# Calculate predicted values
entry_predicted <- entry_blups
entry_predicted$Predicted_Mean <- overall_mean + entry_predicted$BLUP
entry_predicted <- entry_predicted[order(-entry_predicted$Predicted_Mean), ]

cat("Top 10 ENTRIES by Predicted Mean:\n")
print(head(entry_predicted, 10))

# ============================================================================
# STEP 11: MODEL FIT STATISTICS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 11: Model Fit Statistics\n")
cat("============================================================================\n")

# Extract model fit statistics
logLik_val <- logLik(model_random)
AIC_val <- AIC(model_random)
BIC_val <- BIC(model_random)
deviance_val <- -2 * logLik_val

cat("Random Effects Model:\n")
cat("======================================\n")
cat(sprintf("Log-likelihood: %.4f\n", logLik_val))
cat(sprintf("Deviance (-2LL): %.4f\n", deviance_val))
cat(sprintf("AIC: %.4f\n", AIC_val))
cat(sprintf("BIC: %.4f\n", BIC_val))
cat(sprintf("Number of observations: %d\n", nobs(model_random)))
cat(sprintf("Number of groups: ENV=%d, ENTRY=%d\n", 
            length(unique(cas$ENV)), length(unique(cas$ENTRY))))

# ============================================================================
# STEP 12: SUMMARY ANOVA TABLE (Combined)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 12: Combined ANOVA Summary Table\n")
cat("============================================================================\n")
cat("\nFixed Effects (from Model 1):\n")
cat("----------------------------------------------------------------------------\n")
if(exists("anova_table")) {
  print(anova_table, row.names = FALSE)
}

cat("\nRandom Effects (from Model 2):\n")
cat("----------------------------------------------------------------------------\n")
cat(sprintf("%-25s %12s %12s %12s\n", "Component", "Variance", "Std.Dev.", "% Total"))
cat("----------------------------------------------------------------------------\n")
for(i in 1:nrow(var_comp)) {
  if(!is.na(var_comp$grp[i])) {
    cat(sprintf("%-25s %12.4f %12.4f %11.2f%%\n", 
                var_comp$grp[i], 
                var_comp$vcov[i], 
                sqrt(var_comp$vcov[i]),
                var_comp$percent[i]))
  }
}
cat(sprintf("%-25s %12.4f %12.4f %11.2f%%\n", 
            "Residual", 
            residual_var, 
            sqrt(residual_var),
            residual_percent))

# ============================================================================
# STEP 13: VARIANCE COMPONENT PLOT
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 13: Generating Variance Components Plot\n")
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

# Create bar plot
pdf("variance_components.pdf", width = 10, height = 6)
par(mar = c(10, 4, 4, 2))
bp <- barplot(vc_plot_data$Percentage, 
              names.arg = vc_plot_data$Component,
              las = 2,  # Vertical labels
              col = rainbow(nrow(vc_plot_data)),
              main = "Variance Components as Percentage of Total Variance",
              ylab = "Percentage of Total Variance (%)",
              ylim = c(0, max(vc_plot_data$Percentage) * 1.1))
text(x = bp, y = vc_plot_data$Percentage + 2, 
     labels = paste0(round(vc_plot_data$Percentage, 1), "%"))
dev.off()
cat("✓ Variance components plot saved to variance_components.pdf\n")

# ============================================================================
# STEP 14: MODEL DIAGNOSTICS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 14: Model Diagnostics\n")
cat("============================================================================\n")

# Extract residuals
residuals_model <- resid(model_random)
fitted_values <- fitted(model_random)

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
# STEP 15: EXPORT RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 15: Exporting Results\n")
cat("============================================================================\n")

# Export ANOVA table
if(exists("anova_table")) {
  write.csv(anova_table, "anova_table.csv", row.names = FALSE)
  cat("✓ ANOVA table exported to anova_table.csv\n")
}

# Export variance components
write.csv(vc_plot_data, "variance_components.csv", row.names = FALSE)
cat("✓ Variance components exported to variance_components.csv\n")

# Export BLUPs
write.csv(entry_blups, "entry_blups.csv", row.names = FALSE)
cat("✓ ENTRY BLUPs exported to entry_blups.csv\n")

# Export predicted means
write.csv(entry_predicted, "entry_predicted_means.csv", row.names = FALSE)
cat("✓ ENTRY predicted means exported to entry_predicted_means.csv\n")

# Export model fit statistics
fit_stats <- data.frame(
  Statistic = c("LogLik", "Deviance", "AIC", "BIC", "Nobs"),
  Value = c(logLik_val, deviance_val, AIC_val, BIC_val, nobs(model_random))
)
write.csv(fit_stats, "model_fit_statistics.csv", row.names = FALSE)
cat("✓ Model fit statistics exported to model_fit_statistics.csv\n")

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

