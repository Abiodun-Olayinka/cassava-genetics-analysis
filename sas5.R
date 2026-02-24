# Cassava Data Analysis - R Script
# Mixed Model Analysis for Multiple Traits

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
# STEP 2: IDENTIFY TRAITS FOR ANALYSIS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 2: Identifying Traits for Analysis\n")
cat("============================================================================\n")

# List of all traits in the dataset (excluding identifier columns)
all_traits <- c("NOHAV", "RTNO", "SHTWT", "RTWT", "DM", "STARCH", "FYLD", 
                "DYLD", "TYLD", "HI", "NDVI3", "NDVI6", "NDVI9", "LODG", 
                "PLTHTIII", "BRNHTIII", "BRNLEVIII", "PLTHTVI", "BRNHTVI", 
                "BRNLEVVI", "PLTHTIX", "BRNHTIX", "BRNLEVIX", "CHL3", "CHL6", 
                "CHL9", "CHL12", "ANGBR9", "STMDI9", "PPSTD9")

# Check which traits actually exist in the dataset
existing_traits <- intersect(all_traits, names(data_raw))
cat("Number of traits found:", length(existing_traits), "\n")
cat("Traits to be analyzed:\n")
print(existing_traits)

# ============================================================================
# STEP 3: CREATE BASE ANALYSIS DATASET
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 3: Creating Base Analysis Dataset\n")
cat("============================================================================\n")

# Create base dataset with identifier columns
base_data <- data_raw %>%
  dplyr::select(ENV, ENTRY, BLK, REP, dplyr::all_of(existing_traits)) %>%
  dplyr::mutate(
    ENV = as.factor(ENV),
    ENTRY = as.factor(ENTRY),
    BLK = as.factor(BLK),
    REP = as.factor(REP)
  )

cat("Base dataset structure:\n")
str(base_data)

# ============================================================================
# STEP 4: CREATE INTERACTION TERMS (will be used for all traits)
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 4: Creating Interaction Terms\n")
cat("============================================================================\n")

# Create interaction terms
base_data$ENV_ENTRY <- interaction(base_data$ENV, base_data$ENTRY, sep = ":")
base_data$ENV_REP_BLK <- interaction(base_data$ENV, base_data$REP, base_data$BLK, sep = ":")
base_data$ENV_REP <- interaction(base_data$ENV, base_data$REP, sep = ":")

cat("Interaction terms created successfully\n")

# ============================================================================
# STEP 5: FUNCTION TO ADD SIGNIFICANCE STARS
# ============================================================================

add_signif <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.1) return(".")
  return(" ")
}

# ============================================================================
# STEP 6: FUNCTION TO TEST RANDOM EFFECT FOR A GIVEN TRAIT
# ============================================================================

test_random_term <- function(trait_name, data, term_name, full_formula, reduced_formula) {
  tryCatch({
    # Fit full model
    full_model <- lmer(as.formula(full_formula), data = data, REML = FALSE,
                       control = lmerControl(optimizer = "bobyqa", 
                                             optCtrl = list(maxfun = 100000)))
    
    # Fit reduced model
    reduced_model <- lmer(as.formula(reduced_formula), data = data, REML = FALSE,
                          control = lmerControl(optimizer = "bobyqa", 
                                                optCtrl = list(maxfun = 100000)))
    
    # LRT
    lrt <- anova(reduced_model, full_model, refit = FALSE)
    
    # Extract statistics
    chisq <- lrt$Chisq[2]
    df <- lrt$Df[2]
    p_value <- lrt$`Pr(>Chisq)`[2]
    
    # Get variance component
    vc <- as.data.frame(VarCorr(full_model))
    var_comp <- vc$vcov[vc$grp == term_name]
    if(length(var_comp) == 0) var_comp <- NA
    
    return(list(chisq = chisq, df = df, p_value = p_value, var = var_comp, 
                model = full_model))
    
  }, error = function(e) {
    return(list(chisq = NA, df = NA, p_value = NA, var = NA, model = NULL))
  })
}

# ============================================================================
# STEP 7: FUNCTION TO ANALYZE A SINGLE TRAIT
# ============================================================================

analyze_trait <- function(trait_name, data) {
  
  cat("\n----------------------------------------------------------------------------\n")
  cat("Analyzing Trait:", trait_name, "\n")
  cat("----------------------------------------------------------------------------\n")
  
  # Remove rows with missing values for this trait
  trait_data <- data %>%
    dplyr::filter(!is.na(!!sym(trait_name))) %>%
    dplyr::rename(TRAIT = !!sym(trait_name))
  
  cat("Observations:", nrow(trait_data), "\n")
  
  if(nrow(trait_data) < 50) {
    cat("WARNING: Insufficient data for reliable analysis\n")
    return(NULL)
  }
  
  # Define model formulas
  full_formula <- paste("TRAIT ~ (1 | ENV) + (1 | ENTRY) + (1 | ENV_REP_BLK) + 
                        (1 | ENV_REP) + (1 | ENV_ENTRY)")
  
  # Test each random effect
  results <- list()
  
  # Test ENTRY
  cat("  Testing ENTRY...\n")
  entry_test <- test_random_term(trait_name, trait_data, "ENTRY",
                                 full_formula,
                                 paste("TRAIT ~ (1 | ENV) + (1 | ENV_REP_BLK) + 
                                       (1 | ENV_REP) + (1 | ENV_ENTRY)"))
  results$ENTRY <- entry_test
  
  # Test ENV:ENTRY
  cat("  Testing ENV:ENTRY...\n")
  entryenv_test <- test_random_term(trait_name, trait_data, "ENV_ENTRY",
                                    full_formula,
                                    paste("TRAIT ~ (1 | ENV) + (1 | ENTRY) + 
                                          (1 | ENV_REP_BLK) + (1 | ENV_REP)"))
  results$ENV_ENTRY <- entryenv_test
  
  # Test ENV_REP_BLK
  cat("  Testing ENV:REP:BLK...\n")
  blk_test <- test_random_term(trait_name, trait_data, "ENV_REP_BLK",
                               full_formula,
                               paste("TRAIT ~ (1 | ENV) + (1 | ENTRY) + 
                                     (1 | ENV_REP) + (1 | ENV_ENTRY)"))
  results$ENV_REP_BLK <- blk_test
  
  # Test ENV_REP
  cat("  Testing ENV:REP...\n")
  rep_test <- test_random_term(trait_name, trait_data, "ENV_REP",
                               full_formula,
                               paste("TRAIT ~ (1 | ENV) + (1 | ENTRY) + 
                                     (1 | ENV_REP_BLK) + (1 | ENV_ENTRY)"))
  results$ENV_REP <- rep_test
  
  # Test ENV
  cat("  Testing ENV...\n")
  env_test <- test_random_term(trait_name, trait_data, "ENV",
                               full_formula,
                               paste("TRAIT ~ (1 | ENTRY) + (1 | ENV_REP_BLK) + 
                                     (1 | ENV_REP) + (1 | ENV_ENTRY)"))
  results$ENV <- env_test
  
  # Get full model for variance components
  full_model <- lmer(as.formula(full_formula), data = trait_data, REML = TRUE,
                     control = lmerControl(optimizer = "bobyqa"))
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(full_model))
  residual_var <- sigma(full_model)^2
  
  # Calculate total variance
  total_var <- sum(vc$vcov, na.rm = TRUE) + residual_var
  vc$percent <- (vc$vcov / total_var) * 100
  residual_percent <- (residual_var / total_var) * 100
  
  # Create ANOVA table
  anova_table <- data.frame(
    Term = c("ENTRY", "ENTRY:ENV", "BLK:REP:ENV", "REP:ENV", "ENV", "Residual"),
    Df = NA,
    SumSq = NA,
    MeanSq = NA,
    F_value = NA,
    Pr = NA,
    Signif = NA,
    stringsAsFactors = FALSE
  )
  
  # Map results to ANOVA table
  term_mapping <- list(
    "ENTRY" = "ENTRY",
    "ENV_ENTRY" = "ENTRY:ENV",
    "ENV_REP_BLK" = "BLK:REP:ENV",
    "ENV_REP" = "REP:ENV",
    "ENV" = "ENV"
  )
  
  for(term in names(term_mapping)) {
    idx <- which(anova_table$Term == term_mapping[[term]])
    test_result <- results[[term]]
    
    if(!is.null(test_result) && !is.na(test_result$p_value)) {
      anova_table$Df[idx] <- test_result$df
      anova_table$SumSq[idx] <- test_result$var * test_result$df
      anova_table$MeanSq[idx] <- test_result$var
      anova_table$F_value[idx] <- test_result$chisq / test_result$df
      anova_table$Pr[idx] <- test_result$p_value
      anova_table$Signif[idx] <- add_signif(test_result$p_value)
    }
  }
  
  # Residual
  anova_table$Df[6] <- nrow(trait_data) - sum(anova_table$Df[1:5], na.rm = TRUE)
  anova_table$SumSq[6] <- residual_var * anova_table$Df[6]
  anova_table$MeanSq[6] <- residual_var
  
  # Round numeric columns
  numeric_cols <- c("Df", "SumSq", "MeanSq", "F_value", "Pr")
  anova_table[, numeric_cols] <- round(anova_table[, numeric_cols], 4)
  
  # Format p-values
  anova_table$Pr <- format.pval(anova_table$Pr, digits = 4, eps = 0.0001)
  
  # Extract BLUPs for ENTRY
  entry_blups <- ranef(full_model)$ENTRY
  if(!is.null(entry_blups)) {
    entry_blups <- data.frame(
      ENTRY = rownames(entry_blups),
      BLUP = round(entry_blups[,1], 4)
    )
    entry_blups <- entry_blups[order(-entry_blups$BLUP), ]
  } else {
    entry_blups <- NULL
  }
  
  # Calculate heritability
  entry_var <- vc$vcov[vc$grp == "ENTRY"]
  entry_env_var <- vc$vcov[vc$grp == "ENV_ENTRY"]
  if(length(entry_env_var) == 0) entry_env_var <- 0
  
  if(length(entry_var) > 0 && entry_var > 0) {
    avg_reps <- mean(table(trait_data$ENTRY))
    heritability <- entry_var / (entry_var + entry_env_var + residual_var/avg_reps)
  } else {
    heritability <- NA
    avg_reps <- NA
  }
  
  # Return results
  return(list(
    trait = trait_name,
    n_obs = nrow(trait_data),
    anova = anova_table,
    variance_components = vc,
    residual_var = residual_var,
    residual_percent = residual_percent,
    total_var = total_var,
    entry_blups = entry_blups,
    heritability = heritability,
    avg_reps = avg_reps,
    entry_var = ifelse(length(entry_var) > 0, entry_var, NA),
    entry_env_var = entry_env_var,
    model = full_model
  ))
}

# ============================================================================
# STEP 8: RUN ANALYSIS FOR ALL TRAITS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 8: Running Analysis for All Traits\n")
cat("============================================================================\n")

# Initialize results list
all_results <- list()

# Analyze each trait
for(i in seq_along(existing_traits)) {
  trait <- existing_traits[i]
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Processing trait", i, "of", length(existing_traits), ":", trait, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  result <- analyze_trait(trait, base_data)
  if(!is.null(result)) {
    all_results[[trait]] <- result
  }
}

# ============================================================================
# STEP 9: SUMMARY TABLE OF ALL TRAITS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 9: Summary Table of All Traits\n")
cat("============================================================================\n")

# Create summary dataframe
summary_table <- data.frame(
  Trait = character(),
  N = integer(),
  Vg = numeric(),
  Vge = numeric(),
  Ve = numeric(),
  Vg_percent = numeric(),
  Vge_percent = numeric(),
  Ve_percent = numeric(),
  H2 = numeric(),
  stringsAsFactors = FALSE
)

for(trait in names(all_results)) {
  res <- all_results[[trait]]
  
  # Extract variances
  vg <- ifelse(!is.na(res$entry_var), res$entry_var, 0)
  vge <- res$entry_env_var
  ve <- res$residual_var
  
  # Calculate percentages
  total <- vg + vge + ve
  vg_pct <- (vg / total) * 100
  vge_pct <- (vge / total) * 100
  ve_pct <- (ve / total) * 100
  
  summary_table <- rbind(summary_table, data.frame(
    Trait = trait,
    N = res$n_obs,
    Vg = round(vg, 4),
    Vge = round(vge, 4),
    Ve = round(ve, 4),
    Vg_percent = round(vg_pct, 2),
    Vge_percent = round(vge_pct, 2),
    Ve_percent = round(ve_pct, 2),
    H2 = round(res$heritability, 4),
    stringsAsFactors = FALSE
  ))
}

# Print summary table
cat("\nVariance Components Summary Across All Traits:\n")
cat("================================================================================\n")
cat(sprintf("%-15s %6s %8s %8s %8s %8s %8s %8s %6s\n", 
            "Trait", "N", "Vg", "Vge", "Ve", "Vg%", "Vge%", "Ve%", "H²"))
cat("================================================================================\n")
for(i in 1:nrow(summary_table)) {
  cat(sprintf("%-15s %6d %8.4f %8.4f %8.4f %7.1f%% %7.1f%% %7.1f%% %6.4f\n", 
              summary_table$Trait[i],
              summary_table$N[i],
              summary_table$Vg[i],
              summary_table$Vge[i],
              summary_table$Ve[i],
              summary_table$Vg_percent[i],
              summary_table$Vge_percent[i],
              summary_table$Ve_percent[i],
              summary_table$H2[i]))
}
cat("================================================================================\n")

# ============================================================================
# STEP 10: SIGNIFICANCE SUMMARY FOR ALL TRAITS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 10: Significance Summary for All Traits\n")
cat("============================================================================\n")

# Create significance matrix
signif_matrix <- data.frame(Trait = character())

for(trait in names(all_results)) {
  res <- all_results[[trait]]
  signif_row <- data.frame(Trait = trait)
  
  for(term in c("ENTRY", "ENTRY:ENV", "BLK:REP:ENV", "REP:ENV", "ENV")) {
    term_idx <- which(res$anova$Term == term)
    if(length(term_idx) > 0 && !is.na(res$anova$Signif[term_idx])) {
      signif_row[[term]] <- res$anova$Signif[term_idx]
    } else {
      signif_row[[term]] <- "NS"
    }
  }
  
  signif_matrix <- rbind(signif_matrix, signif_row)
}

cat("\nSignificance of Random Effects Across Traits:\n")
cat("================================================================================\n")
print(signif_matrix, row.names = FALSE)
cat("================================================================================\n")
cat("NS = Not Significant, .=p<0.1, *=p<0.05, **=p<0.01, ***=p<0.001\n")

# ============================================================================
# STEP 11: EXPORT ALL RESULTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 11: Exporting Results\n")
cat("============================================================================\n")

# Create results directory
dir.create("cassava_analysis_results", showWarnings = FALSE)
setwd("cassava_analysis_results")

# Export summary table
write.csv(summary_table, "00_all_traits_summary.csv", row.names = FALSE)
cat("✓ All traits summary saved to: 00_all_traits_summary.csv\n")

# Export significance matrix
write.csv(signif_matrix, "00_significance_matrix.csv", row.names = FALSE)
cat("✓ Significance matrix saved to: 00_significance_matrix.csv\n")

# Export individual trait results
for(trait in names(all_results)) {
  res <- all_results[[trait]]
  
  # Create trait-specific folder
  trait_folder <- gsub("[^A-Za-z0-9]", "", trait)
  dir.create(trait_folder, showWarnings = FALSE)
  
  # Export ANOVA table
  write.csv(res$anova, file.path(trait_folder, paste0(trait, "_anova.csv")), 
            row.names = FALSE)
  
  # Export variance components
  vc_export <- data.frame(
    Component = c(res$variance_components$grp, "Residual"),
    Variance = c(res$variance_components$vcov, res$residual_var),
    StdDev = c(sqrt(res$variance_components$vcov), sqrt(res$residual_var)),
    Percent = c(res$variance_components$percent, res$residual_percent)
  )
  vc_export <- vc_export[!is.na(vc_export$Component), ]
  write.csv(vc_export, file.path(trait_folder, paste0(trait, "_varcomp.csv")), 
            row.names = FALSE)
  
  # Export BLUPs
  if(!is.null(res$entry_blups)) {
    write.csv(res$entry_blups, file.path(trait_folder, paste0(trait, "_blups.csv")), 
              row.names = FALSE)
  }
  
  # Export trait-specific summary
  trait_summary <- data.frame(
    Trait = trait,
    N = res$n_obs,
    Total_Variance = res$total_var,
    Heritability = res$heritability,
    Avg_Reps = res$avg_reps
  )
  write.csv(trait_summary, file.path(trait_folder, paste0(trait, "_summary.csv")), 
            row.names = FALSE)
  
  cat("✓ Results for", trait, "saved to", trait_folder, "/\n")
}

# Return to original directory
setwd("..")

# ============================================================================
# STEP 12: GENERATE SUMMARY PLOTS
# ============================================================================

cat("\n============================================================================\n")
cat("STEP 12: Generating Summary Plots\n")
cat("============================================================================\n")

pdf("cassava_analysis_results/all_traits_summary_plots.pdf", width = 12, height = 8)

# Plot 1: Variance components for all traits
par(mfrow = c(1, 2), mar = c(10, 4, 4, 2))

# Heritability plot
barplot(summary_table$H2, 
        names.arg = summary_table$Trait,
        las = 2,
        col = "lightblue",
        main = "Broad-sense Heritability (H²) Across Traits",
        ylab = "Heritability",
        ylim = c(0, 1))
abline(h = 0.3, col = "red", lty = 2)
abline(h = 0.5, col = "orange", lty = 2)
abline(h = 0.7, col = "green", lty = 2)
legend("topright", legend = c("Low", "Medium", "High"), 
       col = c("red", "orange", "green"), lty = 2, cex = 0.8)

# Variance components stacked barplot
barplot(t(as.matrix(summary_table[, c("Vg_percent", "Vge_percent", "Ve_percent")])),
        names.arg = summary_table$Trait,
        las = 2,
        col = c("darkgreen", "orange", "lightgray"),
        main = "Variance Components Across Traits",
        ylab = "Percentage of Total Variance (%)",
        legend.text = c("Vg (Genotype)", "Vge (G×E)", "Ve (Error)"),
        args.legend = list(x = "topright", cex = 0.8))

dev.off()
cat("✓ Summary plots saved to: cassava_analysis_results/all_traits_summary_plots.pdf\n")

# ============================================================================
# STEP 13: FINAL SUMMARY
# ============================================================================

cat("\n============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("============================================================================\n")
cat("Number of traits analyzed:", length(all_results), "\n")
cat("Results saved in: cassava_analysis_results/\n")
cat("\nFiles created:\n")
cat("  - 00_all_traits_summary.csv: Summary of all traits\n")
cat("  - 00_significance_matrix.csv: Significance levels for all traits\n")
cat("  - all_traits_summary_plots.pdf: Summary plots\n")
cat("  - Individual folders for each trait with detailed results\n")
cat("\nDate/Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("============================================================================\n")

