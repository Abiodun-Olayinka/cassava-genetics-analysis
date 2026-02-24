#!/usr/bin/env Rscript
#===============================================================================
# Path Coefficient Analysis for Cassava Yield Traits
# Analysis: Structural Equation Modeling / Path Analysis
# Data: Mokwa location trial data
#===============================================================================

#===============================================================================
# 1. SETUP AND PACKAGE MANAGEMENT
#===============================================================================

# Function to install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Required packages for path analysis
required_packages <- c(
  "lavaan",      # For SEM and path analysis (version 0.6-12)
  "semPlot",     # For visualizing path models (version 1.1.6)
  "OpenMx",      # Alternative SEM package (version 2.21.1)
  "tidyverse",   # For data manipulation (version 1.3.2)
  "knitr",       # For table generation (version 1.41)
  "kableExtra",  # For formatted tables (version 1.3.4)
  "GGally"       # For extended ggplot2 (version 2.1.2)
)

# Install and load all packages
cat("\n=== INSTALLING AND LOADING PACKAGES ===\n")
install_if_missing(required_packages)

# Record package versions for reproducibility
sink("docs/package_versions.txt")
print(sessionInfo())
sink()

#===============================================================================
# 2. DATA IMPORT
#===============================================================================

cat("\n=== LOADING DATA ===\n")

# Define data path - UPDATE THIS PATH FOR YOUR SYSTEM
data_path <- "data/mokwa_data.csv"

# Check if file exists
if (!file.exists(data_path)) {
  stop(paste("Data file not found at:", data_path, 
             "\nPlease update the path or place the data file in the data/ directory"))
}

# Read the data
cassava <- read.csv(data_path)

# Display data structure
cat("\nData dimensions:", dim(cassava), "\n")
cat("\nColumn names:\n")
print(colnames(cassava))

cat("\nFirst few rows of data:\n")
print(head(cassava))

cat("\nData summary:\n")
print(summary(cassava))

#===============================================================================
# 3. PATH MODEL SPECIFICATION
#===============================================================================

cat("\n=== SPECIFYING PATH MODEL ===\n")

# Model specification based on theoretical relationships
# FYLD: Fresh yield (primary outcome)
# Direct effects on FYLD: RTWT, NOHAV, RTNO, HI, LODG, PLTHT6, PLTHT9, STARCH, STMDI9, ANGBR9
# RTWT is also predicted by HI, PLTHT6, PLTHT9, LODG

model <- '
# Direct effects on fresh yield
FYLD ~ RTWT + NOHAV + RTNO + HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9

# Root weight model
RTWT ~ HI + PLTHT6 + PLTHT9 + LODG

# Correlations among exogenous variables (optional)
# HI ~~ PLTHT6 + PLTHT9 + LODG
# PLTHT6 ~~ PLTHT9 + LODG
# PLTHT9 ~~ LODG
'

cat("\nModel specification:\n")
cat(model)

#===============================================================================
# 4. MODEL FITTING
#===============================================================================

cat("\n=== FITTING PATH MODEL ===\n")

# Fit the model using lavaan
fit <- cfa(model, data = cassava)

# Check for convergence
if (fit@Fit@converged) {
  cat("\n✓ Model converged successfully\n")
} else {
  warning("Model did not converge. Check specification and data.")
}

#===============================================================================
# 5. MODEL RESULTS
#===============================================================================

cat("\n=== MODEL SUMMARY ===\n")

# Full summary with fit measures
summary_fit <- summary(fit, 
                       fit.measures = TRUE, 
                       standardized = TRUE, 
                       rsquare = TRUE)

# Extract key results for reporting
cat("\n--- FIT INDICES ---\n")
fit_measures <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
print(fit_measures)

cat("\n--- PARAMETER ESTIMATES ---\n")
parameter_estimates <- parameterEstimates(fit, standardized = TRUE)
print(parameter_estimates)

cat("\n--- R-SQUARED VALUES ---\n")
r_squared <- inspect(fit, "r2")
print(r_squared)

#===============================================================================
# 6. VISUALIZATION
#===============================================================================

cat("\n=== GENERATING PATH DIAGRAMS ===\n")

# Create output directory if it doesn't exist
if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
}

# Path diagram 1: Circle layout
png("outputs/figures/path_diagram_circle.png", width = 12, height = 10, units = "in", res = 300)
semPaths(fit, 
         what = "std",           # Show standardized estimates
         layout = "circle",       # Circular layout
         edge.label.cex = 0.8,    # Edge label size
         fade = FALSE,            # Don't fade non-significant paths
         residuals = TRUE,        # Show residuals
         curvePivot = TRUE)       # Curve paths for clarity
title("Path Diagram: Cassava Yield Determinants (Circle Layout)", line = 3)
dev.off()

# Path diagram 2: Tree layout
png("outputs/figures/path_diagram_tree.png", width = 14, height = 10, units = "in", res = 300)
semPaths(fit, 
         what = "std",            # Show standardized estimates
         layout = "tree",          # Tree layout
         edge.label.cex = 0.9,     # Edge label size
         curvePivot = TRUE,        # Curve paths
         nCharNodes = 0,           # Don't truncate node names
         sizeMan = 8,              # Size of manifest variables
         fade = FALSE)             # Don't fade
title("Path Diagram: Cassava Yield Determinants (Tree Layout)", line = 3)
dev.off()

# Path diagram 3: Simplified for publication
png("outputs/figures/path_diagram_publication.png", width = 10, height = 8, units = "in", res = 300)
semPaths(fit, 
         what = "std",
         layout = "spring",        # Spring layout for better spacing
         edge.label.cex = 0.8,
         curvePivot = TRUE,
         nCharNodes = 4,           # Truncate to 4 characters
         sizeMan = 7,
         fade = FALSE,
         mar = c(2, 2, 2, 2))
dev.off()

cat("\n✓ Path diagrams saved to outputs/figures/\n")

#===============================================================================
# 7. EXPORT RESULTS TABLES
#===============================================================================

cat("\n=== EXPORTING RESULTS ===\n")

# Create output directory for tables
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

# Export parameter estimates
write.csv(parameter_estimates, "outputs/tables/parameter_estimates.csv", row.names = FALSE)

# Export fit indices
fit_indices_df <- as.data.frame(t(fit_measures))
write.csv(fit_indices_df, "outputs/tables/fit_indices.csv", row.names = TRUE)

# Export R-squared values
r2_df <- as.data.frame(r_squared)
colnames(r2_df) <- "R_squared"
write.csv(r2_df, "outputs/tables/r_squared_values.csv", row.names = TRUE)

cat("\n✓ Results tables saved to outputs/tables/\n")

#===============================================================================
# 8. GENERATE MARKDOWN REPORT
#===============================================================================

cat("\n=== GENERATING REPORT ===\n")

# Create a simple text report
sink("outputs/analysis_report.txt")
cat("PATH ANALYSIS REPORT\n")
cat("===================\n\n")
cat("Date:", date(), "\n\n")
cat("DATA SUMMARY\n")
cat("-----------\n")
cat("Number of observations:", nrow(cassava), "\n")
cat("Number of variables:", ncol(cassava), "\n\n")
cat("MODEL FIT\n")
cat("---------\n")
print(fit_measures)
cat("\n\nR-SQUARED VALUES\n")
cat("----------------\n")
print(r_squared)
cat("\n\nSIGNIFICANT PATHS (p < 0.05)\n")
cat("----------------------------\n")
significant <- parameter_estimates[parameter_estimates$pvalue < 0.05, ]
print(significant[, c("lhs", "op", "rhs", "est", "se", "pvalue", "std.all")])
sink()

cat("\n✓ Report saved to outputs/analysis_report.txt\n")

cat("\n=== ANALYSIS COMPLETE ===\n")