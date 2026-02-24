############################################################
# Correlation Plot of Genotypic Correlations
# Data file: data/MOKWA META.csv
############################################################

# Load libraries
library(readxl)
library(corrplot)

# CREATE OUTPUT DIRECTORIES FIRST (THIS IS THE KEY FIX)
if(!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
  cat("✅ Created outputs/tables directory\n")
}

if(!dir.exists("outputs/figures")) {
  dir.create("outputs/figures", recursive = TRUE)
  cat("✅ Created outputs/figures directory\n")
}

# Read the data
data <- read.csv("data/MOKWA META.csv", check.names = FALSE)

# Check if data loaded correctly
if(is.null(data)) {
  stop("❌ Could not load data file. Check if 'data/MOKWA META.csv' exists.")
}

cat("📊 Data loaded successfully. Dimensions:", dim(data), "\n")

# Extract correlation matrix
cor_matrix <- as.matrix(data[, -1])
rownames(cor_matrix) <- data[[1]]

# Verify matrix
cat("Correlation matrix size:", nrow(cor_matrix), "x", ncol(cor_matrix), "\n")

# Create correlation plot
png("outputs/figures/correlation_plot.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "circle",
         type = "lower",
         order = "original",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         col = colorRampPalette(c("red", "white", "blue"))(200),
         addCoef.col = "black",
         number.cex = 0.7,
         cl.pos = "r",
         mar = c(0, 0, 2, 0))
title("Genotypic Correlations Between Cassava Traits", line = 1)
dev.off()
cat("✅ Figure saved to outputs/figures/correlation_plot.png\n")

# Save correlation matrix (NOW THIS WILL WORK)
write.csv(cor_matrix, "outputs/tables/correlation_matrix.csv")
cat("✅ Correlation matrix saved to outputs/tables/correlation_matrix.csv\n")

# Show first few rows
cat("\nFirst 5 rows of correlation matrix:\n")
print(round(cor_matrix[1:min(5, nrow(cor_matrix)), 1:min(5, ncol(cor_matrix))], 3))