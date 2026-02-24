# Cassava Data Analysis - R Script
# Mixed Model Analysis with Complete ANOVA Table

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
cas$ENV_ENTRY <- interaction(cas$ENV, cas$ENTRY, sep = ":")
cas$ENV_REP_BLK <- interaction(cas$ENV, cas$REP, cas$BLK, sep = ":")
cas$ENV_REP <- interaction(cas$ENV, cas$REP, sep = ":")

cat("Interaction terms created successfully\n")

# ============================================================================
# STEP 4: FIT THE FULL MODEL WITH ALL TERMS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 4: Fitting Full Mixed Model\n")
cat("============================================================================\n")

# Check if we have enough data to fit the model
if(nrow(cas) < 10) {
  stop("Insufficient data to fit the model")
}

# Fit the full model with all terms as random effects
# This matches the SAS RANDOM statement
tryCatch({
  full_model <- lmer(FYLD ~ 
                       (1 | ENV) + 
                       (1 | ENTRY) + 
                       (1 | ENV:REP:BLK) + 
                       (1 | ENV:REP) + 
                       (1 | ENV:ENTRY),
                     data = cas,
                     REML = TRUE,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 100000)))
  
  cat("✓ Full model fitted successfully\n\n")
  
}, error = function(e) {
  cat("✗ Error fitting full model:", e$message, "\n")
  cat("Attempting to fit with alternative structure...\n")
  
  # Try alternative model structure if full model fails
  full_model <<- lmer(FYLD ~ 
                        (1 | ENV) + 
                        (1 | ENTRY) + 
                        (1 | ENV/REP/BLK) + 
                        (1 | ENV:ENTRY),
                      data = cas,
                      REML = TRUE,
                      control = lmerControl(optimizer = "bobyqa"))
  
  cat("✓ Alternative model fitted successfully\n")
})

# Display model summary
cat("\nModel Summary:\n")
print(summary(full_model))

# ============================================================================
# STEP 5: CONSTRUCT ANOVA TABLE WITH ALL TERMS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 5: ANOVA Table\n")
cat("============================================================================\n")

# Function to add significance stars
add_signif <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.1) return(".")
  return(" ")
}

# Extract variance components
vc <- as.data.frame(VarCorr(full_model))
residual_var <- sigma(full_model)^2

# Calculate approximate degrees of freedom for each term
# Note: In mixed models with all random effects, we use variance components
# and likelihood ratio tests to assess significance

# Create the ANOVA table structure
anova_output <- data.frame(
  Term = c("ENTRY", "ENTRY:ENV", "BLK:REP:ENV", "REP:ENV", "ENV", "Residual"),
  Df = NA,
  SumSq = NA,
  MeanSq = NA,
  F_value = NA,
  Pr = NA,
  Signif = NA,
  stringsAsFactors = FALSE
)

# Perform likelihood ratio tests for each random effect
# This gives us p-values for significance testing

# Function to perform LRT and extract statistics
test_random_term <- function(term_name, full_model, reduced_formula) {
  tryCatch({
    # Fit reduced model
    reduced <- lmer(reduced_formula, data = cas, REML = FALSE)
    full_ml <- lmer(formula(full_model), data = cas, REML = FALSE)
    
    # LRT
    lrt <- anova(reduced, full_ml, refit = FALSE)
    
    # Extract statistics
    chisq <- lrt$Chisq[2]
    df <- lrt$Df[2]
    p_value <- lrt$`Pr(>Chisq)`[2]
    
    # Approximate SumSq and MeanSq (these are approximations)
    # In mixed models, we don't get exact SumSq for random effects
    # We use the variance component as a proxy
    var_comp <- vc$vcov[vc$grp == term_name]
    if(length(var_comp) == 0) var_comp <- NA
    
    return(list(chisq = chisq, df = df, p_value = p_value, var = var_comp))
    
  }, error = function(e) {
    return(list(chisq = NA, df = NA, p_value = NA, var = NA))
  })
}

# Test ENTRY
entry_test <- test_random_term("ENTRY", full_model, 
                               "FYLD ~ (1 | ENV) + (1 | ENV:REP:BLK) + (1 | ENV:REP) + (1 | ENV:ENTRY)")
if(!is.na(entry_test$p_value)) {
  anova_output[1, "Df"] <- 1
  anova_output[1, "SumSq"] <- entry_test$var * entry_test$df  # Approximate
  anova_output[1, "MeanSq"] <- entry_test$var
  anova_output[1, "F_value"] <- entry_test$chisq / entry_test$df  # Approximate
  anova_output[1, "Pr"] <- entry_test$p_value
  anova_output[1, "Signif"] <- add_signif(entry_test$p_value)
}

# Test ENTRY:ENV
entryenv_test <- test_random_term("ENV:ENTRY", full_model,
                                  "FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | ENV:REP:BLK) + (1 | ENV:REP)")
if(!is.na(entryenv_test$p_value)) {
  anova_output[2, "Df"] <- 1
  anova_output[2, "SumSq"] <- entryenv_test$var * entryenv_test$df
  anova_output[2, "MeanSq"] <- entryenv_test$var
  anova_output[2, "F_value"] <- entryenv_test$chisq / entryenv_test$df
  anova_output[2, "Pr"] <- entryenv_test$p_value
  anova_output[2, "Signif"] <- add_signif(entryenv_test$p_value)
}

# Test BLK:REP:ENV
blk_test <- test_random_term("ENV:REP:BLK", full_model,
                             "FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | ENV:REP) + (1 | ENV:ENTRY)")
if(!is.na(blk_test$p_value)) {
  anova_output[3, "Df"] <- 1
  anova_output[3, "SumSq"] <- blk_test$var * blk_test$df
  anova_output[3, "MeanSq"] <- blk_test$var
  anova_output[3, "F_value"] <- blk_test$chisq / blk_test$df
  anova_output[3, "Pr"] <- blk_test$p_value
  anova_output[3, "Signif"] <- add_signif(blk_test$p_value)
}

# Test REP:ENV
rep_test <- test_random_term("ENV:REP", full_model,
                             "FYLD ~ (1 | ENV) + (1 | ENTRY) + (1 | ENV:REP:BLK) + (1 | ENV:ENTRY)")
if(!is.na(rep_test$p_value)) {
  anova_output[4, "Df"] <- 1
  anova_output[4, "SumSq"] <- rep_test$var * rep_test$df
  anova_output[4, "MeanSq"] <- rep_test$var
  anova_output[4, "F_value"] <- rep_test$chisq / rep_test$df
  anova_output[4, "Pr"] <- rep_test$p_value
  anova_output[4, "Signif"] <- add_signif(rep_test$p_value)
}

# Test ENV
env_test <- test_random_term("ENV", full_model,
                             "FYLD ~ (1 | ENTRY) + (1 | ENV:REP:BLK) + (1 | ENV:REP) + (1 | ENV:ENTRY)")
if(!is.na(env_test$p_value)) {
  anova_output[5, "Df"] <- 1
  anova_output[5, "SumSq"] <- env_test$var * env_test$df
  anova_output[5, "MeanSq"] <- env_test$var
  anova_output[5, "F_value"] <- env_test$chisq / env_test$df
  anova_output[5, "Pr"] <- env_test$p_value
  anova_output[5, "Signif"] <- add_signif(env_test$p_value)
}

# Residual
anova_output[6, "Df"] <- nrow(cas) - sum(anova_output$Df[1:5], na.rm = TRUE)
anova_output[6, "SumSq"] <- residual_var * anova_output[6, "Df"]
anova_output[6, "MeanSq"] <- residual_var

# Round numeric columns
numeric_cols <- c("Df", "SumSq", "MeanSq", "F_value", "Pr")
anova_output[, numeric_cols] <- round(anova_output[, numeric_cols], 4)

# Format p-values
anova_output$Pr <- format.pval(anova_output$Pr, digits = 4, eps = 0.0001)

# Print the ANOVA table in the requested format
cat("\n")
cat("                 Df   Sum Sq   Mean Sq  F value    Pr(>F)    \n")
cat("----------------------------------------------------------------\n")
for(i in 1:nrow(anova_output)) {
  if(!is.na(anova_output$Df[i])) {
    cat(sprintf("%-20s %3.0f %8.4f %8.4f %8.4f %10s %s\n", 
                anova_output$Term[i],
                anova_output$Df[i],
                anova_output$SumSq[i],
                anova_output$MeanSq[i],
                anova_output$F_value[i],
                anova_output$Pr[i],
                anova_output$Signif[i]))
  } else {
    cat(sprintf("%-20s %3s %8s %8s %8s %10s %s\n", 
                anova_output$Term[i], "", "", "", "", "", ""))
  }
}
cat("----------------------------------------------------------------\n")
cat("---\n")
cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

# ============================================================================
# STEP 6: VARIANCE COMPONENTS TABLE
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 6: Variance Components\n")
cat("============================================================================\n")

# Calculate total variance
total_var <- sum(vc$vcov, na.rm = TRUE) + residual_var
vc$percent <- (vc$vcov / total_var) * 100
residual_percent <- (residual_var / total_var) * 100

# Print variance components
cat("\nVariance Components:\n")
cat("========================================\n")
cat(sprintf("%-20s %12s %12s %12s\n", "Component", "Variance", "Std.Dev.", "% Total"))
cat("----------------------------------------\n")
for(i in 1:nrow(vc)) {
  if(!is.na(vc$grp[i])) {
    cat(sprintf("%-20s %12.4f %12.4f %11.2f%%\n", 
                vc$grp[i], 
                vc$vcov[i], 
                sqrt(vc$vcov[i]),
                vc$percent[i]))
  }
}
cat(sprintf("%-20s %12.4f %12.4f %11.2f%%\n", 
            "Residual", 
            residual_var, 
            sqrt(residual_var),
            residual_percent))
cat("========================================\n")

# ============================================================================
# STEP 7: HERITABILITY CALCULATION
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 7: Genetic Parameters\n")
cat("============================================================================\n")

# Extract ENTRY variance
entry_var <- vc$vcov[vc$grp == "ENTRY"]
if(length(entry_var) > 0) {
  # Average number of replicates per genotype
  avg_reps <- mean(table(cas$ENTRY))
  
  # Broad-sense heritability
  # H² = Vg / (Vg + Vge + Ve/r)
  entry_env_var <- vc$vcov[vc$grp == "ENV:ENTRY"]
  if(length(entry_env_var) == 0) entry_env_var <- 0
  
  heritability <- entry_var / (entry_var + entry_env_var + residual_var/avg_reps)
  
  cat(sprintf("\nAverage replicates per genotype: %.2f\n", avg_reps))
  cat(sprintf("Genotypic variance (Vg): %.4f\n", entry_var))
  cat(sprintf("Genotype x Environment variance (Vge): %.4f\n", entry_env_var))
  cat(sprintf("Residual variance (Ve): %.4f\n", residual_var))
  cat(sprintf("\nBroad-sense heritability (H²): %.4f (%.2f%%)\n", 
              heritability, heritability*100))
}

# ============================================================================
# STEP 8: BLUPs FOR ENTRIES
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 8: BLUPs for ENTRIES\n")
cat("============================================================================\n")

# Extract BLUPs for ENTRY
entry_blups <- ranef(full_model)$ENTRY
if(!is.null(entry_blups)) {
  entry_blups <- data.frame(
    ENTRY = rownames(entry_blups),
    BLUP = round(entry_blups[,1], 4)
  )
  
  # Sort by BLUP
  entry_blups <- entry_blups[order(-entry_blups$BLUP), ]
  
  cat("\nTop 10 ENTRIES by BLUP:\n")
  print(head(entry_blups, 10))
  
  cat("\nBottom 10 ENTRIES by BLUP:\n")
  print(tail(entry_blups, 10))
}

# ============================================================================
# STEP 9: EXPORT RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 9: Exporting Results\n")
cat("============================================================================\n")

# Export ANOVA table
write.csv(anova_output, "anova_table.csv", row.names = FALSE)
cat("✓ ANOVA table exported to anova_table.csv\n")

# Export variance components
vc_export <- data.frame(
  Component = c(vc$grp, "Residual"),
  Variance = c(vc$vcov, residual_var),
  StdDev = c(sqrt(vc$vcov), sqrt(residual_var)),
  Percent = c(vc$percent, residual_percent)
)
vc_export <- vc_export[!is.na(vc_export$Component), ]
write.csv(vc_export, "variance_components.csv", row.names = FALSE)
cat("✓ Variance components exported to variance_components.csv\n")

# Export BLUPs
if(exists("entry_blups")) {
  write.csv(entry_blups, "entry_blups.csv", row.names = FALSE)
  cat("✓ ENTRY BLUPs exported to entry_blups.csv\n")
}

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Date/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

