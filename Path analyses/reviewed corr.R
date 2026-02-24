# R Script to Import Data and Analyze Correlations for Specific Traits

# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot")) install.packages("corrplot")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")

library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)

# Define the traits of interest
target_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", 
                   "STARCH", "BRNHB9", "ANGBR9", "STMDI9", "PPSTD9", 
                   "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", 
                   "DYLD", "TYLD", "HI", "DM", "BRNHT6")

# Function to import and prepare data
import_data <- function(file_path) {
  # Read the data file
  data <- read.csv("IKENNE.csv")
 # data <- read_csv(file_path)
  
  # Check if all target traits exist in the data
  missing_traits <- setdiff(target_traits, names(data))
  if (length(missing_traits) > 0) {
    warning("The following traits were not found in the data: ", 
            paste(missing_traits, collapse = ", "))
  }
  
  # Select only the traits that exist in the data
  existing_traits <- intersect(target_traits, names(data))
  
  # Convert to numeric (in case some are read as character)
  data <- data %>%
    mutate(across(all_of(existing_traits), as.numeric))
  
  return(list(data = data, traits_used = existing_traits))
}

# Function to run and plot correlations
analyze_correlations <- function(data, traits, method = "pearson") {
  # Subset data to selected traits
  data_sub <- data %>% select(all_of(traits))
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_sub, use = "pairwise.complete.obs", method = method)
  
  # Calculate p-values for correlations
  p_matrix <- rcorr(as.matrix(data_sub), type = method)$P
  
  # Print summary information
  cat("\nAnalyzing", length(traits), "traits:", paste(traits, collapse = ", "), "\n")
  cat("\nCorrelation method:", method, "\n")
  
  # Plot 1: Correlation matrix with corrplot
  corrplot(cor_matrix, 
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           diag = FALSE,
           title = paste("Correlation Matrix (", toupper(method), ")", sep = ""),
           mar = c(0, 0, 2, 0))
  
  # Plot 2: Heatmap with ggplot2
  melted_cor <- melt(cor_matrix)
  
  heatmap_plot <- ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = paste(toupper(method), "\nCorrelation")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    labs(x = "", y = "", 
         title = paste("Correlation Matrix of", length(traits), "Traits"))
  
  print(heatmap_plot)
  
  # Return results as a list
  invisible(list(cor_matrix = cor_matrix, 
                 p_matrix = p_matrix,
                 traits_analyzed = traits,
                 method = method))
}

# Main analysis workflow
run_analysis <- function(file_path, method = "pearson") {
  # Step 1: Import and prepare data
  imported <- import_data(file_path)
  data <- imported$data
  traits_to_analyze <- imported$traits_used
  
  # Step 2: Run correlation analysis
  results <- analyze_correlations(data, traits = traits_to_analyze, method = method)
  
  # Step 3: Return all results
  return(results)
}

# Example usage:
# Replace "your_data_file.csv" with the path to your actual data file
# results <- run_analysis("your_data_file.csv", method = "pearson")

# For testing with a simulated dataset (comment out in real use)
set.seed(123)
test_data <- as.data.frame(matrix(rnorm(20*100), ncol = 20))
colnames(test_data) <- target_traits
write_csv(test_data, "test_data.csv")

# Run the analysis (use this line with your real data)
analysis_results <- run_analysis("test_data.csv")  # Replace with your file path

# To access results:
# analysis_results$cor_matrix  # Correlation matrix
# analysis_results$p_matrix    # P-values
# analysis_results$traits_analyzed  # Which traits were actually analyzed





















# Load required packages
library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)

# Define the traits of interest
target_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", 
                   "STARCH", "BRNHB9", "ANGBR9", "STMDI9", "PPSTD9", 
                   "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", 
                   "DYLD", "TYLD", "HI", "DM", "BRNHT6")

# Function to import and prepare data
import_data <- function(file_path) {
  # Read the data file - using read.csv instead of read_csv for base R compatibility
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Check if all target traits exist in the data
  missing_traits <- setdiff(target_traits, names(data))
  if (length(missing_traits) > 0) {
    warning("The following traits were not found in the data: ", 
            paste(missing_traits, collapse = ", "))
  }
  
  # Select only the traits that exist in the data
  existing_traits <- intersect(target_traits, names(data))
  
  # Convert to numeric (in case some are read as character)
  data <- data %>%
    mutate(across(all_of(existing_traits), as.numeric))
  
  return(list(data = data, traits_used = existing_traits))
}

# Function to run and plot correlations with improved plotting
analyze_correlations <- function(data, traits, method = "pearson") {
  # Subset data to selected traits
  data_sub <- data %>% select(all_of(traits))
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_sub, use = "pairwise.complete.obs", method = method)
  
  # Calculate p-values for correlations
  p_matrix <- rcorr(as.matrix(data_sub), type = method)$P
  
  # Print summary information
  cat("\nAnalyzing", length(traits), "traits:", paste(traits, collapse = ", "), "\n")
  cat("\nCorrelation method:", method, "\n")
  
  # Create a layout for multiple plots
  par(mfrow = c(1, 1)) # Reset to single plot layout
  
  # Plot 1: Correlation matrix with corrplot (full matrix)
  corrplot(cor_matrix, 
           method = "color",
           type = "full",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.7,
           diag = TRUE,
           title = paste("Correlation Matrix (", toupper(method), ")", sep = ""),
           mar = c(0, 0, 2, 0))
  
  # Plot 2: Heatmap with ggplot2
  melted_cor <- melt(cor_matrix)
  
  heatmap_plot <- ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = paste(toupper(method), "\nCorrelation")) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid = element_blank()) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 2.5) +
    labs(x = "", y = "", 
         title = paste("Correlation Heatmap of", length(traits), "Traits"))
  
  # Plot 3: Significance plot
  melted_p <- melt(p_matrix)
  sig_plot <- ggplot(melted_p, aes(Var1, Var2, fill = value < 0.05)) +
    geom_tile() +
    scale_fill_manual(values = c("white", "gray"), 
                      name = "Significant\n(p < 0.05)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "", title = "Statistical Significance of Correlations")
  
  # Display all plots
  print(heatmap_plot)
  print(sig_plot)
  
  # Return results as a list
  invisible(list(cor_matrix = cor_matrix, 
                 p_matrix = p_matrix,
                 traits_analyzed = traits,
                 method = method))
}

# Main analysis workflow
run_analysis <- function(file_path, method = "pearson") {
  # Step 1: Import and prepare data
  imported <- import_data(file_path)
  data <- imported$data
  traits_to_analyze <- imported$traits_used
  
  # Step 2: Run correlation analysis
  results <- analyze_correlations(data, traits = traits_to_analyze, method = method)
  
  # Step 3: Return all results
  return(results)
}

# Run the analysis with your actual data file
analysis_results <- run_analysis("IKENNE.csv")

# To access results:
# analysis_results$cor_matrix  # Correlation matrix
# analysis_results$p_matrix    # P-values
# analysis_results$traits_analyzed  # Which traits were actually analyzed
























# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot")) install.packages("corrplot")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")

library(tidyverse)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(PerformanceAnalytics)

# Define the traits of interest
target_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", 
                   "STARCH", "BRNHB9", "ANGBR9", "STMDI9", "PPSTD9", 
                   "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", 
                   "DYLD", "TYLD", "HI", "DM", "BRNHT6")

# Function to import and prepare data
import_data <- function(file_path) {
  # Read the data file
  data <- read_csv(file_path)
  
  # Check if all target traits exist in the data
  missing_traits <- setdiff(target_traits, names(data))
  if (length(missing_traits) > 0) {
    warning("The following traits were not found in the data: ", 
            paste(missing_traits, collapse = ", "))
  }
  
  # Select only the traits that exist in the data
  existing_traits <- intersect(target_traits, names(data))
  
  # Convert to numeric (in case some are read as character)
  data <- data %>%
    mutate(across(all_of(existing_traits), as.numeric))
  
  return(list(data = data, traits_used = existing_traits))
}

# Function to create multiple correlation plots with significance
create_correlation_plots <- function(cor_matrix, p_matrix, method) {
  # Set up plot layout
  par(mfrow = c(1, 1)) # Reset after plotting
  
  # Plot 1: Basic corrplot with significance levels
  corrplot(cor_matrix, 
           method = "circle",
           type = "upper",
           order = "hclust",
           p.mat = p_matrix,
           sig.level = c(0.001, 0.01, 0.05),
           insig = "label_sig",
           pch.cex = 0.8,
           tl.col = "black",
           tl.srt = 45,
           title = paste("Correlation with Significance Levels (", toupper(method), ")"),
           mar = c(0, 0, 2, 0))
  
  # Plot 2: Number corrplot with colored background
  corrplot(cor_matrix,
           method = "number",
           type = "full",
           order = "original",
           col = colorRampPalette(c("blue", "white", "red"))(20),
           p.mat = p_matrix,
           sig.level = 0.05,
           insig = "blank",
           tl.col = "black",
           tl.cex = 0.7,
           number.cex = 0.6,
           title = paste("Correlation Coefficients (", toupper(method), ")"),
           mar = c(0, 0, 2, 0))
  
  # Plot 3: ggcorrplot with hierarchical clustering
  ggcorrplot(cor_matrix,
             hc.order = TRUE,
             type = "lower",
             outline.color = "white",
             ggtheme = ggplot2::theme_minimal,
             colors = c("#6D9EC1", "white", "#E46726"),
             lab = TRUE,
             p.mat = p_matrix,
             sig.level = 0.05,
             insig = "blank") +
    labs(title = "Hierarchical Clustering Correlation (p < 0.05)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 4: PerformanceAnalytics correlation chart
  chart.Correlation(as.matrix(data %>% select(all_of(traits))), 
                    histogram = TRUE, 
                    pch = 19,
                    method = method)
  
  # Plot 5: Significance heatmap
  melted_p <- melt(p_matrix)
  ggplot(melted_p, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                         midpoint = 0.05, limit = c(0, 0.1),
                         name = "P-value") +
    geom_text(aes(label = ifelse(value < 0.05, 
                                 format.pval(value, eps = .001, digits = 2),
                                 "")),
              color = "black", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    coord_fixed() +
    labs(x = "", y = "", title = "P-values Matrix")
}

# Main analysis function
run_analysis <- function(file_path, method = "pearson") {
  # Step 1: Import and prepare data
  imported <- import_data(file_path)
  data <- imported$data
  traits <- imported$traits_used
  
  # Step 2: Calculate correlation and p-values
  cor_matrix <- cor(data %>% select(all_of(traits)), 
                    use = "pairwise.complete.obs", 
                    method = method)
  p_matrix <- rcorr(as.matrix(data %>% select(all_of(traits))), type = method)$P
  
  # Print summary
  cat("\nAnalyzing", length(traits), "traits:", paste(traits, collapse = ", "), "\n")
  cat("Correlation method:", method, "\n\n")
  
  # Create all plots
  create_correlation_plots(cor_matrix, p_matrix, method)
  
  # Return results
  invisible(list(cor_matrix = cor_matrix,
                 p_matrix = p_matrix,
                 traits_analyzed = traits))
}

# Example usage with your data
analysis_results <- run_analysis("IKENNE.csv")

# To save all plots to files (uncomment to use)
# pdf("correlation_analysis.pdf", width = 10, height = 8)
# analysis_results <- run_analysis("IKENNE.csv")
# dev.off()





# Load necessary libraries
library(corrplot)

# Load your dataset (replace 'your_data.csv' with your actual file)
data <- read.csv("your_data.csv", header = TRUE)

# Select only the specified traits for correlation analysis
selected_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", "STARCH", "BRNHB9", "ANGBR9", "STMDI9", 
                     "PPSTD9", "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", "DYLD", "TYLD", "HI", "DM", "BRNHT6")
numeric_data <- data[selected_traits]

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)



























# Load necessary libraries
library(corrplot)
library(Hmisc)
library(ggplot2)
library(reshape2)

# Load your dataset (replace 'your_data.csv' with your actual file)
data <- read.csv("your_data.csv", header = TRUE)

# Select only the specified traits for correlation analysis
selected_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", "STARCH", "BRNHB9", "ANGBR9", "STMDI9", 
                     "PPSTD9", "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", "DYLD", "TYLD", "HI", "DM", "BRNHT6")
numeric_data <- data[selected_traits]

# Compute correlation matrix with significance testing
cor_results <- rcorr(as.matrix(numeric_data), type = "pearson")
cor_matrix <- cor_results$r
p_matrix <- cor_results$P

# Function to flatten correlation matrix with p-values
flattenCorrMatrix <- function(cor_mat, p_mat) {
  ut <- upper.tri(cor_mat)
  data.frame(
    row = rownames(cor_mat)[row(cor_mat)[ut]],
    column = colnames(cor_mat)[col(cor_mat)[ut]],
    cor = (cor_mat)[ut],
    p = p_mat[ut]
  )
}
cor_data <- flattenCorrMatrix(cor_matrix, p_matrix)

# Create different types of correlation plots
# 1. Standard corrplot with significance levels
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,
         p.mat = p_matrix, sig.level = 0.05, insig = "blank")

# 2. Circle plot similar to the attached image
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7,
         p.mat = p_matrix, sig.level = 0.05, insig = "blank")

# 3. Heatmap-style correlation plot
corrplot(cor_matrix, method = "shade", type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, col = colorRampPalette(c("blue", "white", "red"))(200))

# 4. ggplot2 alternative for a publication-ready heatmap
cor_data_melt <- melt(cor_data, id.vars = c("row", "column"))
ggplot(cor_data_melt, aes(x = row, y = column, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "Traits", y = "Traits")































# Load necessary libraries
library(corrplot)
library(Hmisc)
library(ggplot2)
library(ggcorrplot)

# Load your dataset (replace 'your_data.csv' with your actual file)
data <- read.csv("IKENNE.csv", header = TRUE)
data <- read.csv("MOKWA.csv", header = TRUE)
data <- read.csv("ONNE.csv", header = TRUE)

# Select only the specified traits for correlation analysis
selected_traits <- c("LODG", "BRNLEV9", "PLTHT9", "NOHAV", "RTNO", "STARCH", "BRNHB9", "ANGBR9", "STMDI9", 
                     "PPSTD9", "SHTWT", "RTWT", "BRNHT9", "PLTHT6", "FYLD", "DYLD", "TYLD", "HI", "DM", "BRNHT6")
numeric_data <- data[selected_traits]

# Compute correlation matrix and p-values
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
cor_test <- rcorr(as.matrix(numeric_data), type = "pearson")

# 1. Basic correlation plot with coefficients
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, title = "Basic Correlation Matrix")

# 2. Correlation plot with significance levels (p-values)
corrplot(cor_matrix, p.mat = cor_test$P, sig.level = c(0.001, 0.01, 0.05), 
         insig = "label_sig", pch.cex = 0.8, pch.col = "white",
         method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         title = "Correlation with Significance Levels")

# 3. Mixed correlation plot (color + circle)
corrplot.mixed(cor_matrix, lower = "circle", upper = "color", 
               tl.pos = "lt", diag = "n", tl.col = "black",
               p.mat = cor_test$P, sig.level = 0.05, insig = "blank",
               title = "Mixed Correlation Plot")

# 4. Hierarchical clustering correlation plot
corrplot(cor_matrix, method = "color", type = "full", order = "hclust",
         hclust.method = "complete", addrect = 4, rect.col = "black",
         tl.col = "black", tl.srt = 45, 
         p.mat = cor_test$P, sig.level = 0.05, insig = "blank",
         title = "Hierarchical Clustering Correlation")

# 5. ggcorrplot with p-values (similar to ggplot style)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",
           outline.color = "white", lab = TRUE, lab_size = 3,
           p.mat = cor_test$P, sig.level = 0.05, insig = "blank") +
  ggtitle("GGPlot Style Correlation") +
  theme(plot.title = element_text(hjust = 0.5))

# 6. Numbered correlation plot with significance
corrplot(cor_matrix, method = "number", type = "upper",
         p.mat = cor_test$P, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45, diag = FALSE,
         title = "Numbered Correlation with Significance")

# 7. Color gradient correlation plot with significance stars
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         p.mat = cor_test$P, sig.level = c(0.001, 0.01, 0.05), 
         insig = "label_sig", pch.cex = 0.8, pch.col = "white",
         tl.col = "black", tl.srt = 45,
         title = "Color Gradient with Significance Stars")

# Save all plots to PDF (optional)
pdf("correlation_plots.pdf", width = 10, height = 10)
# Repeat your favorite plot commands here
dev.off()


# GGPlot Style Correlation (upper triangle only with coefficients)
ggcorrplot(cor_matrix, 
           hc.order = FALSE,          # Disable hierarchical clustering
           type = "upper",            # Show only upper triangle
           outline.color = "white",   # White outline for cells
           lab = TRUE,                # Show correlation coefficients
           lab_size = 3,              # Size of coefficient text
           colors = c("#6D9EC1", "white", "#E46726"),  # Blue-white-red gradient
           p.mat = cor_test$P,        # p-values for significance
           sig.level = 0.05,          # Significance level
           insig = "blank",           # Blank out non-significant correlations
           tl.cex = 10,              # Text label size
           tl.srt = 45,               # Rotate text labels 45 degrees
           digits = 2) +              # Round coefficients to 2 decimals
  ggtitle("GGPlot Style Correlation Matrix") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right") +
  scale_fill_gradient2(low = "#6D9EC1", high = "#E46726", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation")






# GGPlot Style Correlation (upper triangle only with coefficients) - X-axis labels on top
ggcorrplot(cor_matrix, 
           hc.order = FALSE,          # Disable hierarchical clustering
           type = "upper",            # Show only upper triangle
           outline.color = "white",   # White outline for cells
           lab = TRUE,                # Show correlation coefficients
           lab_size = 3,              # Size of coefficient text
           colors = c("#6D9EC1", "white", "#E46726"),  # Blue-white-red gradient
           p.mat = cor_test$P,        # p-values for significance
           sig.level = 0.05,          # Significance level
           insig = "blank",           # Blank out non-significant correlations
           tl.cex = 10,               # Text label size
           digits = 2) +              # Round coefficients to 2 decimals
  ggtitle("GGPlot Style Correlation Matrix") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0, margin = margin(b = 0)),  # X-axis on top
    axis.text.y = element_text(margin = margin(r = 0)),
    axis.ticks = element_blank(),
    legend.position = "right",
    # Move x-axis to top and adjust spacing
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  # This moves the x-axis labels to the top
  scale_x_discrete(position = "top") +
  scale_fill_gradient2(
    low = "#6D9EC1", 
    high = "#E46726", 
    mid = "white", 
    midpoint = 0, 
    limit = c(-1,1), 
    space = "Lab",
    name = "Pearson\nCorrelation"
  )



# GGPlot Style Correlation (upper triangle) - Shows NS for non-significant correlations
ggcorrplot(cor_matrix, 
           hc.order = FALSE,          # Disable hierarchical clustering
           type = "upper",            # Show only upper triangle
           outline.color = "white",   # White outline for cells
           lab = TRUE,                # Show correlation coefficients
           lab_size = 3,              # Size of coefficient text
           colors = c("#6D9EC1", "white", "#E46726"),  # Blue-white-red gradient
           p.mat = cor_test$P,        # p-values for significance
           sig.level = 0.05,          # Significance level
           insig = "label_sig",       # Label non-significant with "NS"
           pch = "NS",                # Label for non-significant
           pch.cex = 3,               # Size of NS label
           pch.col = "gray40",        # Color for NS label
           tl.cex = 10,               # Text label size
           digits = 2) +              # Round coefficients to 2 decimals
  ggtitle("Correlation Matrix (NS = Not Significant at p < 0.05)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0, margin = margin(b = 0)),
    axis.text.y = element_text(margin = margin(r = 0)),
    axis.ticks = element_blank(),
    legend.position = "right",
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank()
  ) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient2(
    low = "#6D9EC1", 
    high = "#E46726", 
    mid = "white", 
    midpoint = 0, 
    limit = c(-1,1), 
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  # Add annotation to explain NS
  labs(caption = "NS = Not Significant (p ≥ 0.05)") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10, color = "gray40"))



# Load required libraries
library(ggcorrplot)

# Compute correlation matrix and p-values
cor_matrix <- cor(data, use = "complete.obs")
cor_test <- cor.mtest(data)

# GGPlot Style Correlation Plot (full matrix with coefficients)
ggcorrplot(cor_matrix,
           hc.order = FALSE,          # Disable hierarchical clustering
           type = "full",             # Show full matrix (upper and lower triangle)
           outline.color = "white",   # White outline for cells
           lab = TRUE,                # Show correlation coefficients
           lab_size = 3,              # Size of coefficient text
           colors = c("red", "white", "blue"),  # Red-white-blue gradient
           p.mat = cor_test$P,        # p-values for significance
           sig.level = 0.05,          # Significance level
           insig = "blank",           # Blank out non-significant correlations
           tl.cex = 10,               # Text label size
           digits = 2) +              # Round coefficients to 2 decimals
  ggtitle("Correlation Matrix") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust y-axis labels
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  # Adjust color gradient for better visualization
  scale_fill_gradient2(
    low = "red", 
    high = "blue", 
    mid = "white", 
    midpoint = 0, 
    limit = c(-1, 1), 
    space = "Lab",
    name = "Pearson\nCorrelation"
  )





# Load required libraries
library(ggcorrplot)

# Define a helper function to compute p-values for correlation matrix
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(list(P = p.mat))
}

# Ensure the data is numeric
numeric_data <- data[sapply(data, is.numeric)]

# Compute correlation matrix and p-values
cor_matrix <- cor(numeric_data, use = "complete.obs")
cor_test <- cor.mtest(numeric_data)

# GGPlot Style Correlation Plot (full matrix with coefficients)
ggcorrplot(cor_matrix,
           hc.order = FALSE,          # Disable hierarchical clustering
           type = "full",             # Show full matrix (upper and lower triangle)
           outline.color = "white",   # White outline for cells
           lab = TRUE,                # Show correlation coefficients
           lab_size = 3,              # Size of coefficient text
           colors = c("red", "white", "blue"),  # Red-white-blue gradient
           p.mat = cor_test$P,        # p-values for significance
           sig.level = 0.05,          # Significance level
           insig = "blank",           # Blank out non-significant correlations
           tl.cex = 10,               # Text label size
           digits = 2) +              # Round coefficients to 2 decimals
  ggtitle("Correlation Matrix") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust y-axis labels
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  # Adjust color gradient for better visualization
  scale_fill_gradient2(
    low = "red", 
    high = "blue", 
    mid = "white", 
    midpoint = 0, 
    limit = c(-1, 1), 
    space = "Lab",
    name = "Pearson\nCorrelation"
  )
