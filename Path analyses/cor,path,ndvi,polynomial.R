### Remove last features
rm(list=ls())
ls()

# Load necessary libraries
library(ggplot2)    # for data visualization
library(caret)      # for data preprocessing
library(leaps)      # for stepwise regression
library(Metrics)    # for evaluating model performance
library(reshape2)   # for data manipulation

setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/Polynomial NDVI")

# Load the dataset
cassava_data <- read.csv("IKENNEpath.csv")
View(cassava_data)
# Explore the data
cat("Head of cassava_data:\n")
print(head(cassava_data))
cat("\nSummary of cassava_data:\n")
print(summary(cassava_data))

# Visualize data distribution with color
cassava_data_long <- melt(cassava_data, id.vars = c("FYLD", "NOHAV", "RTNO", "SHTWT", "RTWT", "DM", "STARCH", "FYLD", "DYLD", "TYLD", "HI", "LODG", "ANGBR9", "STMDI9", "PPSTD9"))

# Rename duplicate columns
names(cassava_data_long)[names(cassava_data_long) == "FYLD.1"] <- "FYLD_1"

# Plot distribution of Cassava Traits
ggplot(cassava_data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  ggtitle("Distribution of Cassava Traits") +
  xlab("Traits") +
  ylab("Value") +
  scale_fill_brewer(palette = "Dark2")  # Use a colorful palette

# Remove rows with missing values
cassava_data <- na.omit(cassava_data)

# Perform polynomial regression
poly_reg <- lm(FYLD ~ poly(NDVI, 2) + poly(PLTHTIII, 2) + poly(CHL3, 2) + poly(STMDI9, 2) + poly(DM, 2) + poly(STARCH, 2) + poly(ANGBR9, 2) + poly(RTWT, 2) + poly(SHTWT, 2) + poly(HI, 2) + poly(LODG, 2) + poly(RTNO, 2), data = cassava_data)

# Print the regression summary
cat("\nRegression Summary:\n")
print(summary(poly_reg))

# Make predictions
predictions <- predict(poly_reg, newdata = cassava_data)

# Evaluate model performance
r_squared <- R2(predictions, cassava_data$FYLD)
mae <- mae(predictions, cassava_data$FYLD)
rmse <- rmse(predictions, cassava_data$FYLD)

cat("\nModel Performance Metrics:\n")
cat("R-squared:", r_squared, "\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")

# Visualize actual vs. predicted FYLD
plot(cassava_data$FYLD, predictions, main = "Onne 9 MAP Actual vs. Predicted FYLD", xlab = "Actual FYLD", ylab = "Predicted FYLD")
abline(0, 1, col = "red") # Add a 45-degree line for reference

# Heatmap of correlations
corr_matrix <- cor(cassava_data[, -c(4:20)])
ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  ggtitle("Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("")


# Heatmap of correlations
corr_matrix <- cor(cassava_data[, -c(1:3)])
ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  ggtitle("Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("")

library(corrplot)
library(psych)  # Load the psych package for correlation tests

# Calculate correlations
corr_result <- corr.test(cassava_data[, -c(1:3)])

# Extract correlation matrix and p-values
corr_matrix <- corr_result$r
p_values <- corr_result$p

# Create a diagonal plot with correlation coefficients and p-values
corrplot(corr_matrix, method = "number", type = "lower", number.cex = 0.7, addgrid.col = NA)
corrplot(p_values, method = "p-value", type = "upper", p.mat = p_values, sig.level = 0.05, insig = "blank", addgrid.col = NA)

library(Hmisc)
library(corrplot)

# Compute genotypic and phenotypic correlation matrices
geno_corr <- rcorr(as.matrix(cassava_data[, -c(1:3)]), type = "pearson")
pheno_corr <- rcorr(as.matrix(cassava_data[, -c(1:3)]), type = "spearman")

# Visualize both matrices in a single plot
par(mfrow=c(1,2))
corrplot(geno_corr$r, method = "color", type = "lower", tl.col = "black", title = "Genotypic Correlation", tl.srt = 45)
corrplot(pheno_corr$r, method = "color", type = "lower", tl.col = "black", title = "Phenotypic Correlation", tl.srt = 45)

library(Hmisc)
library(corrplot)

# Compute genotypic and phenotypic correlation matrices
geno_corr <- rcorr(as.matrix(cassava_data[, -c(1:3)]), type = "pearson")
pheno_corr <- rcorr(as.matrix(cassava_data[, -c(1:3)]), type = "spearman")

# Combine upper and lower diagonal matrices
combine_matrices <- function(lower_mat, upper_mat) {
  n <- nrow(lower_mat)
  m <- matrix(NA, nrow = n, ncol = n)
  m[lower.tri(m)] <- lower_mat
  m[upper.tri(m)] <- t(upper_mat)[upper.tri(t(upper_mat))]
  rownames(m) <- colnames(m) <- rownames(lower_mat)
  return(m)
}

# Combine genotypic and phenotypic correlation matrices
combined_corr <- combine_matrices(geno_corr$r, pheno_corr$r)

# Visualize combined matrix
corrplot(combined_corr, method = "color", tl.col = "black", title = "Combined Correlation", tl.srt = 45)

library(lavaan)
library(ggplot2)
library(reshape2)

# Define the model
model <- '
  # Define latent variables
  FYLD =~ NOHAV + RTNO + SHTWT + RTWT + DM + STARCH + DYLD + TYLD + HI + NDVI + LODG + PLTHTIII + CHL3 + ANGBR9 + STMDI9 + PPSTD9

  # Define regression paths
  FYLD ~ 1  # This line specifies FYLD as a dependent variable without predictors
'
library(lavaan)
library(ggplot2)
library(reshape2)

# Define the model
model <- '
FYLD ~ RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'

# Fit the model
fit <- sem(model, data = cassava_data)

# Extract path coefficients
path_coefficients <- parameterEstimates(fit)$est

# Convert to dataframe
path_df <- data.frame(Trait = rownames(path_coefficients), Coefficient = path_coefficients)

# Visualize path coefficients
ggplot(path_df, aes(x = Trait, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Path Coefficients for FYLD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Traits") +
  ylab("Path Coefficient")



model <- '
FYLD ~   RTWT + NOHAV + RTNO +  HI + LODG + PLTHT6 + PLTHT9 + STARCH + STMDI9 + ANGBR9
RTWT ~  HI + PLTHT6 + PLTHT9 + LODG
'

# Fit the model
fit <- sem(model, data = cassava_data)

# Extract path coefficients
path_coefficients <- parameterEstimates(fit)$est

# Visualize path coefficients
path_df <- data.frame(Trait = rownames(path_coefficients), Coefficient = path_coefficients[,1])
ggplot(path_df, aes(x = Trait, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Path Coefficients for FYLD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Traits") +
  ylab("Path Coefficient")


library(lavaan)
library(ggplot2)
library(reshape2)

# Define the model
model <- '
  # Define latent variables
  FYLD =~ FYLD + NOHAV + RTNO + SHTWT + RTWT + DM + STARCH + DYLD + TYLD + HI + NDVI + LODG + PLTHTIII + CHL3 + ANGBR9 + STMDI9 + PPSTD9

  # Define regression paths
  FYLD ~ NOHAV + RTNO + SHTWT + RTWT + DM + STARCH + DYLD + TYLD + HI + NDVI + LODG + PLTHTIII + CHL3 + ANGBR9 + STMDI9 + PPSTD9
'

# Fit the model
fit <- sem(model, data = cassava_data)

# Extract path coefficients
path_coefficients <- parameterEstimates(fit)$est[1,-1]

# Visualize path coefficients
path_df <- data.frame(Trait = names(path_coefficients), Coefficient = path_coefficients)
ggplot(path_df, aes(x = Trait, y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Path Coefficients for FYLD") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Traits") +
  ylab("Path Coefficient")


library(corrplot)

# Calculate correlations
corr_matrix <- cor(cassava_data[, -c(1:3)])
p_values <- cor_pmat(cassava_data[, -c(1:3)]) # Calculate p-values

# Create a diagonal plot with correlation coefficients and p-values
corrplot(corr_matrix, method = "number", type = "lower", number.cex = 0.7, addgrid.col = NA)
corrplot(p_values, method = "p-value", type = "upper", p.mat = p_values, sig.level = 0.05, insig = "blank", addgrid.col = NA)

