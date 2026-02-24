# Install necessary packages if not already installed
# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("ggcorrplot")
# Remove all objects from the environment
rm(list = ls())

variables <- c("ANGBR9", "BRNHB9", "BRNHT9", "BRNHT6", "BRNLEV9", "DM", "DYLD", 
               "FYLD", "HI", "LODG", "NOHAV", "PLTHT9", "PLTHT6", "PPSTD9", 
                 "RTNO", "RTWT", "SHTWT", "STARCH", "STMDI9", "TYLD")

# Load necessary libraries
library(ggplot2)
library(corrplot)
library(ggcorrplot)


variables <- c("ANGBR9", "BRNHB9", "BRNHT9", "BRNHT6", "BRNLEV9", "DM", "DYLD", 
               "FYLD", "HI", "LODG", "NOHAV", "PLTHT9", "PLTHT6", "PPSTD9", 
               "PT", "RTNO", "RTWT", "SHTWT", "STARCH", "STMDI9", "TYLD", "VR")

# Install necessary packages if not already installed
# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("ggcorrplot")

# Load necessary libraries
library(ggplot2)
library(corrplot)
library(ggcorrplot)

# Assuming your data is in a data frame called `data`
# Load your dataset (replace 'your_data.csv' with the actual data file)
 data <- read.csv("variability2.csv")
 data <- read.csv("3ENVIKMKON.csv")
 data <- read.csv("IKENNE.csv")
 data <- read.csv("MOKWA.csv")
 data <- read.csv("ONNE.csv")
# Select the relevant columns for correlation analysis
# Ensure these column names match your dataset exactly
variables <- c("plant_height", "number_of_branches", "level_of_branching", 
               "angle_of_branching", "stem_diameter", "fresh_yield", 
               "dry_yield", "starch", "dry_matter", 
               "height_at_first_branch", "root_weight", "flowering")

# Subset the data to include only these variables
data_subset <- data[variables]

# Step 1: Calculate the correlation matrix
cor_matrix <- cor(data_subset, use = "complete.obs")

# Step 2: Create a correlation plot using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "circle",      # Correlation method for visualization
           type = "lower",         # Show only the lower triangle
           lab = TRUE,             # Display the correlation coefficient
           title = "Correlation Matrix",
           colors = c("red", "white", "blue"))  # Customize the color gradient

# Step 3: Alternatively, you can create a correlation plot using corrplot
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,      # Control the size of the coefficients
         tl.cex = 0.8,          # Text size for variable names
         title = "Correlation Matrix Heatmap")

# Step 4: Create pairwise scatter plots for selected variable pairs
# You can modify this to plot any specific pair of variables
pairs(data_subset,
      main = "Pairwise Scatter Plots",
      pch = 21, bg = "lightblue", 
      col = "black")

# Optional: Plot individual scatterplots with regression lines (for one pair)
ggplot(data, aes(x = plant_height, y = fresh_yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Plant Height vs Fresh Yield",
       x = "Plant Height",
       y = "Fresh Yield") +
  theme_minimal()


# Assuming your data is in a data frame called `data`
# Load your dataset (replace 'your_data.csv' with the actual data file)
# data <- read.csv("your_data.csv")
data <- read.csv("varcor.csv")
# Select the relevant columns for correlation analysis
# Ensure these column names match your dataset exactly

variables <-	c("ANGBR9",	"BRNHB9",	"BRNHT9",	"BRNHT6",	"BRNLEV9",	"DM",	"DYLD", "FYLD",	"HI",	"LODG",	"NOHAV",	"PLTHT9",	"PLTHT6",	"PPSTD9",	"PT",	"RTNO",	"RTWT",	"SHTWT",	"STARCH",	"STMDI9",	"TYLD"	"VR)

#variables <- c("plant_height", "number_of_branches", "level_of_branching", 
               "angle_of_branching", "stem_diameter", "fresh_yield", 
               "dry_yield", "starch", "dry_matter", 
               "height_at_first_branch", "root_weight", "flowering")

Subset the data to include only these variables
data_subset <- data[variables]

# Step 1: Calculate the correlation matrix
cor_matrix <- cor(data_subset, use = "complete.obs")

# Step 2: Create a correlation plot using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "circle",      # Correlation method for visualization
           type = "lower",         # Show only the lower triangle
           lab = TRUE,             # Display the correlation coefficient
           title = "Correlation Matrix",
           colors = c("red", "white", "blue"))  # Customize the color gradient

# Step 3: Alternatively, you can create a correlation plot using corrplot
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,      # Control the size of the coefficients
         tl.cex = 0.8,          # Text size for variable names
         title = "Correlation Matrix Heatmap")

# Step 4: Create pairwise scatter plots for selected variable pairs
# You can modify this to plot any specific pair of variables
pairs(data_subset,
      main = "Pairwise Scatter Plots",
      pch = 21, bg = "lightblue", 
      col = "black")

# Optional: Plot individual scatterplots with regression lines (for one pair)
ggplot(data, aes(x = plant_height, y = fresh_yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Plant Height vs Fresh Yield",
       x = "Plant Height",
       y = "Fresh Yield") +
  theme_minimal()#
  
  Subset the data to include only these variables
data_subset <- data[variables]



