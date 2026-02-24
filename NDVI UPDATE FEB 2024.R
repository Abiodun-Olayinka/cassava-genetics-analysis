# Load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom)
library(broom.mixed)

# Set working directory
setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/NDVI update February 2024")

# Read the data
dat <- read.csv("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/NDVI update February 2024/MOKWA ONNE DATA FOR COR ANALYSIS.csv")

# Check column names and structure
colnames(dat)
table(dat$location, dat$year_harvest)

# Create a combined environment variable and ensure all factors are properly set
dat1 <- dat %>% 
  unite("Env", c(location, year_harvest), remove = FALSE) %>% 
  mutate(across(c(Env, trial, year_harvest, location, accession_name, 
                  plot_number, block_number, is_a_control, rep_number, 
                  row_number, col_number), as.factor))

# Define all traits to analyze - UPDATED TO MATCH YOUR COLUMN NAMES
traits <- c("NDVI3", "NDVI6", "NDVI9", "HI", "LODG", 
            "PLTHTIII", "PLTHTVI", "PLTHTIX",  # Note: using III, VI, IX instead of 3,6,9
            "ANGBR9", "STMDI9", "TYLD", "DYLD", "FYLD", "STARCH", 
            "DM", "RTWT", "SHTWT")

# Create a mapping for display names (optional)
display_names <- c(
  "NDVI3" = "NDVI3",
  "NDVI6" = "NDVI6", 
  "NDVI9" = "NDVI9",
  "HI" = "HI",
  "LODG" = "LODG",
  "PLTHTIII" = "PLTHT3",
  "PLTHTVI" = "PLTHT6",
  "PLTHTIX" = "PLTHT9",
  "ANGBR9" = "ANGBR9",
  "STMDI9" = "STMDI",
  "TYLD" = "TYLD",
  "DYLD" = "DYLD",
  "FYLD" = "FYLD",
  "STARCH" = "STARCH",
  "DM" = "DM",
  "RTWT" = "RTWT",
  "SHTWT" = "SHTWT"
)

# Create a function to run ANOVA and extract the specific components
run_anova_components <- function(trait_name, data) {
  
  # Skip if trait doesn't exist in data
  if(!trait_name %in% colnames(data)) {
    cat("\nTrait", trait_name, "not found in data. Available traits:\n")
    print(colnames(data)[grep(paste0(str_sub(trait_name, 1, 4)), colnames(data), ignore.case = TRUE)])
    return(NULL)
  }
  
  # Remove rows with missing values for this trait
  data_clean <- data %>% 
    filter(!is.na(!!sym(trait_name))) %>%
    droplevels()
  
  # Check if we have enough data
  if(nrow(data_clean) < 10) {
    cat("\nInsufficient data for", trait_name, "\n")
    return(NULL)
  }
  
  cat("\n========================================\n")
  cat("Processing trait:", trait_name, "\n")
  cat("Number of observations:", nrow(data_clean), "\n")
  cat("Number of environments:", length(unique(data_clean$Env)), "\n")
  cat("Number of genotypes:", length(unique(data_clean$accession_name)), "\n")
  cat("Number of reps:", length(unique(data_clean$rep_number)), "\n")
  cat("========================================\n")
  
  # Fit the mixed model with all components
  formula_str <- paste0(trait_name, " ~ Env + (1|Env:rep_number) + (1|Env:rep_number:block_number) + (1|accession_name) + (1|Env:accession_name)")
  
  model <- tryCatch({
    lmer(as.formula(formula_str), data = data_clean, 
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
  }, error = function(e) {
    message(paste("Error fitting full model for", trait_name, ":", e$message))
    
    # Try without block if block structure might be problematic
    tryCatch({
      simpler_formula <- paste0(trait_name, " ~ Env + (1|Env:rep_number) + (1|accession_name) + (1|Env:accession_name)")
      lmer(as.formula(simpler_formula), data = data_clean,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
    }, error = function(e2) {
      message(paste("Simpler model also failed for", trait_name, ":", e2$message))
      
      # Try minimal model
      tryCatch({
        minimal_formula <- paste0(trait_name, " ~ Env + (1|accession_name) + (1|Env:accession_name)")
        lmer(as.formula(minimal_formula), data = data_clean,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
      }, error = function(e3) {
        message(paste("Minimal model failed for", trait_name, ":", e3$message))
        return(NULL)
      })
    })
  })
  
  if(is.null(model)) return(NULL)
  
  # Get ANOVA table for fixed effects
  anova_fixed <- tryCatch({
    anova(model, ddf = "Satterthwaite") %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Effect")
  }, error = function(e) {
    # If Satterthwaite fails, try Kenward-Roger
    anova(model, ddf = "Kenward-Roger") %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Effect")
  })
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(model))
  
  # Function to create significance stars
  sig_stars <- function(p_value) {
    if(is.na(p_value)) return("")
    if(p_value < 0.001) return("***")
    if(p_value < 0.01) return("**")
    if(p_value < 0.05) return("*")
    return("ns")
  }
  
  # ENV component (fixed effect)
  env_row <- anova_fixed[anova_fixed$Effect == "Env", ]
  env_value <- if(nrow(env_row) > 0 && !is.na(env_row$`F.value`)) {
    paste0(format(round(env_row$`F.value`, 2), nsmall = 2), 
           sig_stars(env_row$`Pr(>F)`))
  } else {
    "NA"
  }
  
  # REP(ENV) component
  rep_env_var <- vc[vc$grp == "Env:rep_number", "vcov"]
  rep_env_value <- if(length(rep_env_var) > 0 && !is.na(rep_env_var)) {
    format(round(rep_env_var, 3), nsmall = 3)
  } else {
    "0.000"
  }
  
  # BLK(ENV*REP) component
  blk_env_rep_var <- vc[vc$grp == "Env:rep_number:block_number", "vcov"]
  if(length(blk_env_rep_var) == 0 || is.na(blk_env_rep_var)) {
    # Try alternative naming patterns
    blk_env_rep_var <- vc[grepl("block", vc$grp) & grepl("Env", vc$grp) & grepl("rep", vc$grp), "vcov"]
  }
  blk_value <- if(length(blk_env_rep_var) > 0 && !is.na(blk_env_rep_var[1])) {
    format(round(blk_env_rep_var[1], 3), nsmall = 3)
  } else {
    "0.000"
  }
  
  # GEN component
  gen_var <- vc[vc$grp == "accession_name", "vcov"]
  gen_value <- if(length(gen_var) > 0 && !is.na(gen_var)) {
    format(round(gen_var, 3), nsmall = 3)
  } else {
    "0.000"
  }
  
  # GEN * ENV component
  gen_env_var <- vc[vc$grp == "Env:accession_name", "vcov"]
  gen_env_value <- if(length(gen_env_var) > 0 && !is.na(gen_env_var)) {
    format(round(gen_env_var, 3), nsmall = 3)
  } else {
    "0.000"
  }
  
  # Residuals
  resid_var <- vc[vc$grp == "Residual", "vcov"]
  resid_value <- if(length(resid_var) > 0 && !is.na(resid_var)) {
    format(round(resid_var, 3), nsmall = 3)
  } else {
    "0.000"
  }
  
  # Model fit statistics
  logLik_value <- tryCatch(logLik(model) %>% as.numeric() %>% round(2), error = function(e) NA)
  AIC_value <- tryCatch(AIC(model) %>% round(2), error = function(e) NA)
  BIC_value <- tryCatch(BIC(model) %>% round(2), error = function(e) NA)
  
  # Use display name if available
  display_trait <- ifelse(trait_name %in% names(display_names), 
                          display_names[trait_name], 
                          trait_name)
  
  # Create result row
  result <- tibble(
    Trait = display_trait,
    ENV = env_value,
    `BLK(ENV*REP)` = blk_value,
    `REP(ENV)` = rep_env_value,
    GEN = gen_value,
    `GEN * ENV` = gen_env_value,
    Residuals = resid_value,
    LogLik = logLik_value,
    AIC = AIC_value,
    BIC = BIC_value,
    N_obs = nrow(data_clean),
    N_env = length(unique(data_clean$Env)),
    N_gen = length(unique(data_clean$accession_name))
  )
  
  # Print results
  cat("\nVariance Components:\n")
  print(vc[, c("grp", "vcov")])
  
  cat("\nFixed Effects ANOVA:\n")
  print(anova_fixed)
  
  cat("\nExtracted Components for Table:\n")
  print(result)
  
  return(list(
    trait = trait_name,
    display_trait = display_trait,
    table_row = result,
    variance_components = vc,
    anova_fixed = anova_fixed,
    model = model
  ))
}

# Run analysis for all traits
cat("\n\n========== GENERATING ANOVA RESULTS WITH SPECIFIED COMPONENTS ==========\n")
cat("Components: ENV, BLK(ENV*REP), REP(ENV), GEN, GEN * ENV, Residuals\n\n")

results_list <- list()
for(trait in traits) {
  results_list[[trait]] <- run_anova_components(trait, dat1)
}

# Create the formatted ANOVA table
cat("\n\n========== FINAL ANOVA TABLE (Similar to Table 4 in manuscript) ==========\n\n")

# Extract only successful results
successful_results <- results_list[!sapply(results_list, is.null)]

if(length(successful_results) > 0) {
  # Combine all table rows
  anova_table_final <- bind_rows(lapply(successful_results, function(x) x$table_row))
  
  # Print the table
  print(anova_table_final)
  
  # Create a nicely formatted table for export
  formatted_table <- anova_table_final %>%
    select(Trait, ENV, `BLK(ENV*REP)`, `REP(ENV)`, GEN, `GEN * ENV`, Residuals)
  
  cat("\nFormatted ANOVA Table:\n")
  print(formatted_table)
  
  # Export results to CSV
  write_csv(formatted_table, "anova_components_table.csv")
  write_csv(anova_table_final, "anova_components_detailed.csv")
  
  cat("\nResults saved to:\n")
  cat("- anova_components_table.csv (simplified table)\n")
  cat("- anova_components_detailed.csv (detailed results with fit statistics)\n")
  
  # Generate a summary of significance across traits
  cat("\n\nSummary of Significance Patterns:\n")
  significance_summary <- anova_table_final %>%
    mutate(
      ENV_sig = case_when(
        grepl("\\*\\*\\*", ENV) ~ "***",
        grepl("\\*\\*", ENV) ~ "**", 
        grepl("\\*", ENV) ~ "*",
        grepl("ns", ENV) ~ "ns",
        TRUE ~ "NA"
      )
    ) %>%
    select(Trait, ENV_sig) %>%
    arrange(ENV_sig)
  
  print(significance_summary)
  
} else {
  cat("\nNo successful model fits. Check trait names and data quality.\n")
  cat("\nAvailable columns in data:\n")
  print(colnames(dat))
}

# Alternative approach using lmerTest for detailed ANOVA tables
cat("\n\n========== DETAILED ANOVA TABLES FOR KEY TRAITS ==========\n")

key_traits <- c("FYLD", "DM", "STARCH", "HI", "PLTHTIX", "LODG")  # Note: PLTHTIX instead of PLTHT9

for(trait in key_traits) {
  if(trait %in% colnames(dat)) {
    cat("\n\n----- Detailed ANOVA for:", trait, "-----\n")
    
    data_clean <- dat1 %>% 
      filter(!is.na(!!sym(trait))) %>%
      droplevels()
    
    # Fit model
    formula_str <- paste0(trait, " ~ Env + (1|Env:rep_number) + (1|accession_name) + (1|Env:accession_name)")
    model <- lmer(as.formula(formula_str), data = data_clean)
    
    # Get ANOVA table
    anova_table <- anova(model, ddf = "Satterthwaite")
    print(anova_table)
    
    # Get variance components
    vc <- VarCorr(model)
    print(vc)
  }
}
