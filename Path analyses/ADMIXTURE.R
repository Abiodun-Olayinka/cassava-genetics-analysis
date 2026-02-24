# Example R script to plot ADMIXTURE results
library(ggplot2)

#setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/Population Structure Dec 2023")

setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/ADMIXTURE MID MARKERS/combined")
# Load the Q-matrix file
#q_matrix <- read.table("genotypes_admixture.3.Q", header = FALSE)

q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)


# Assign sample names to rows
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Plot the barplot
barplot(t(q_matrix), col = rainbow(3), border = NA, names.arg = rownames(q_matrix), las = 2, main = "Population Structure")
legend("topright", legend = paste("Population", 1:3), fill = rainbow(3), border = NA)

# Save the plot
ggsave("admixture_plot.png", width = 10, height = 6, units = "in")

# Install necessary packages if you haven't
# install.packages("dendextend")

library(dendextend)  # For working with dendrograms

# Read the Q-matrix
#qmatrix <- read.table("MIDMARKERS_filtered.3.Q", header = TRUE, row.names = 1)

# Read the Q-matrix from the MIDMARKERS_filtered.3.Q file
qmatrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)

# View the first few rows of the Q-matrix
head(qmatrix)


# View the first few rows of the Q-matrix
head(qmatrix)

# Calculate the distance matrix
dist_matrix <- dist(qmatrix)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")  # Using Ward's method for clustering

# Plot the dendrogram
plot(as.dendrogram(hc), main = "Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")

# Cut the tree into clusters (e.g., 3 clusters)
clusters <- cutree(hc, k = 3)

# Assign colors to the clusters
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)  # Color branches by cluster

# Plot the colored dendrogram
plot(dend, main = "Colored Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")


# Plot the colored dendrogram
plot(dend, main = "Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")




# Load necessary libraries
library(dendextend)

# Read the Q-matrix
q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Set up the plotting area to have two panels side by side
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Plot the barplot
barplot(t(q_matrix), col = rainbow(ncol(q_matrix)), border = NA, names.arg = rownames(q_matrix), las = 2, main = "Population Structure")

# Calculate the distance matrix
dist_matrix <- dist(q_matrix)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(as.dendrogram(hc), main = "Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")

# Cut the tree into clusters (e.g., 3 clusters)
clusters <- cutree(hc, k = 3)

# Assign colors to the clusters
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)  # Color branches by cluster

# Plot the colored dendrogram
plot(dend, main = "Colored Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")

# Reset plotting area to default single-panel layout
par(mfrow = c(1, 1))

# Load required packages
if (!require("dendextend")) install.packages("dendextend", dependencies=TRUE)
library(dendextend)

# Read the Q-matrix from the file
q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)

# Assign sample names to rows
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Set up the plotting area: 1 row, 2 columns
par(mfrow = c(1, 2))

# Plot the barplot
barplot(t(q_matrix), col = rainbow(3), border = NA, names.arg = rownames(q_matrix), las = 2, main = "Population Structure")

# Calculate the distance matrix
dist_matrix <- dist(q_matrix)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")  # Using Ward's method for clustering

# Plot the dendrogram
dend <- as.dendrogram(hc)

# Cut the tree into clusters (e.g., 3 clusters)
clusters <- cutree(hc, k = 3)

# Color branches by cluster
dend_colored <- color_branches(dend, k = 3) 

# Plot the colored dendrogram
plot(dend_colored, main = "Colored Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")

# Add individual names under the dendrogram plot
# Ensure the plotting area has enough space for the names
plot.new()  # Create a new plot for text
par(mar = c(5, 0, 0, 0))  # Adjust margins to fit text

# Add individual names
text(x = 1:length(rownames(q_matrix)), y = rep(0, length(rownames(q_matrix))), labels = rownames(q_matrix), srt = 90, adj = c(1, 0.5))

# Reset plotting area
par(mfrow = c(1, 1))

# Load libraries
library(ggplot2)
library(gplots)
library(ape)

# Load Q-matrix (Ancestry Proportions) using read.table
# Assuming the file is tab-delimited or space-delimited
Q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = TRUE, sep = "\t")  # Replace "\t" with " " if space-delimited

# Load SNP data or a distance matrix for dendrogram using read.table
genetic_data <- read.table("GENO.hmp.txt", header = TRUE, sep = "\t")  # Adjust sep as necessary

# Set population clusters (assume K = number of clusters)
K <- ncol(Q_matrix)


# Reshape the data
Q_matrix$Individual <- 1:nrow(Q_matrix)
Q_long <- reshape2::melt(Q_matrix, id.vars = "Individual")

# Plot ancestry proportions
structure_plot <- ggplot(Q_long, aes(x = factor(Individual), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = rainbow(K)) + 
  labs(x = "Individuals", y = "Ancestry Proportions") +
  theme_minimal()

print(structure_plot)


# Compute distance matrix (if using SNP data)
distance_matrix <- dist(genetic_data)

# Create hierarchical clustering
hc <- hclust(distance_matrix)

# Convert hclust object to dendrogram
dend <- as.dendrogram(hc)

# Plot dendrogram
dendrogram_plot <- plot(dend, main = "Dendrogram of Genetic Distances")

install.packages("cowplot")
library(cowplot)

# Combine the two plots
combined_plot <- plot_grid(structure_plot, dendrogram_plot, ncol = 1, align = "v", rel_heights = c(1, 1))

print(combined_plot)


library(adegenet)

library(ggplot2) 

  data <- read.table("genotypes_admixture.3.Q", header = FALSE)
  genind <- df2genind(data)
  genind.dapc <- dapc(genind, n.pca = 20)
  pdf("dapc_plot.pdf", width = 8, height = 8)
  scatter(genind.dapc, posi.da = TRUE, cell = 0.8)
  dev.off()
  
  
 
  
  Sure, I can help you rewrite your script to include the DAPC and NJ plots. Here is one possible way to do it:
    
    # Example R script to plot ADMIXTURE results and DAPC and NJ phylogenetic tree
    library(ggplot2)
  
  # Load the Q-matrix file
  q_matrix <- read.table("genotypes_admixture.3.Q", header = FALSE)
  
  # Assign sample names to rows
  rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))
  
  # Plot the barplot
  barplot(t(q_matrix), col = rainbow(3), border = NA, names.arg = rownames(q_matrix), las = 2, main = "Population Structure")
  legend("topright", legend = paste("Population", 1:3), fill = rainbow(3), border = NA)
  
  # Save the plot
  ggsave("admixture_plot.png", width = 10, height = 6, units = "in")
  
  # Load the adegenet package for DAPC and NJ analysis
  library(adegenet)
  
  # Find the optimal number of clusters using find.clusters function
  n_clusters <- find.clusters(q_matrix, kmax = 10)
  
  # Create a dapc object with the optimal number of clusters
  dapc_obj <- dapc(q_matrix, n_clusters = n_clusters)
  
  # Plot the scatterplots of principal components and eigenvalues using scatter.dapc function
  scatter.dapc(dapc_obj, type = "PCA")
  
  # Plot the probabilities of assignment of individuals to different clusters using assignplot function
  assignplot(dapc_obj)
  
  # Load the rpart package for NJ analysis
  library(rpart)
  
  # Train a decision tree model using rpart function with method="class"
  tree_obj <- rpart(q_matrix ~ ., data = q_matrix, method="class")
  
  # Plot the decision tree using rpart.plot function
  rpart.plot(tree_obj)
  
  I hope this helps you with your task. If you have any questions or feedback, please let me know. 😊
  
  Source: Conversation with Bing, 15/12/2023
  (1) A tutorial for Discriminant Analysis of Principal Components (DAPC .... http://adegenet.r-forge.r-project.org/files/tutorial-dapc.pdf.
                                                                    (2) A tutorial for Discriminant Analysis of Principal Components (DAPC .... https://adegenet.r-forge.r-project.org/files/adegenet-dapc.pdf.
                                                                                                                                      (3) Plotting Decision Trees in R with rpart and rpart.plot. https://www.r-bloggers.com/2023/09/plotting-decision-trees-in-r-with-rpart-and-rpart-plot/.
                                                                                                                                      (4) dapc graphics function - RDocumentation. https://www.rdocumentation.org/packages/adegenet/versions/2.1.10/topics/dapc%20graphics.


dapc_obj <- dapc(q_matrix, n_clusters = n_clusters)


# Plot the scatterplots of principal components and eigenvalues using scatter.dapc function
scatter.dapc(dapc_obj, type = "PCA")

Alternatively, you can use the find.clusters.data.frame function from the adegenet package to get a data frame with cluster assignments and other information. For example, you can try:

# Find the optimal number of clusters using find.clusters.data.frame function
n_clusters <- find.clusters.data.frame(q_matrix, kmax = 10)

# Create a dapc object with the optimal number of clusters
dapc_obj <- dapc(q_matrix, n_clusters = n_clusters)

# Plot the scatterplots of principal components and eigenvalues using scatter.dapc function
scatter.dapc(dapc_obj, type = "PCA")

I hope this helps you fix your error. If you need more information or examples on how to use DAPC and related functions in R, you can check out these resources:

- [A tutorial for Discriminant Analysis of Principal Components (DAPC) using adegenet](^1^)
- [dapc function - RDocumentation](^2^)
- [dapcGraphics - R Package Documentation](^3^)
- [dapc : Discriminant Analysis of Principal Components (DAPC)](^4^)
- [R: Error in if ... { : missing value where TRUE/FALSE needed](^5^)

Please let me know if you have any other questions or feedback. I'm always happy to help. 😊
                                                                                                                                      
                                                                                                                                      Source: Conversation with Bing, 15/12/2023
                                                                                                                                      (1) A tutorial for Discriminant Analysis of Principal Components (DAPC .... http://adegenet.r-forge.r-project.org/files/tutorial-dapc.pdf.
                                                                                                                                                                                                        (2) dapc function - RDocumentation. https://rdocumentation.org/packages/adegenet/versions/2.1.10/topics/dapc.
                                                                                                                                                                                                        (3) dapcGraphics - R Package Documentation. https://rdrr.io/cran/adegenet/man/dapcGraphics.html.
                                                                                                                                                                                                        (4) dapc : Discriminant Analysis of Principal Components (DAPC). https://rdrr.io/cran/adegenet/man/dapc.html.
                                                                                                                                                                                                        (5) R: Error in if ... { : missing value where TRUE/FALSE needed. https://stackoverflow.com/questions/65819506/r-error-in-if-missing-value-where-true-false-needed.                                                                                                                              (5) dapc_plot : Plot DAPC results - R Package Documentation. https://rdrr.io/github/j-a-thia/genomalicious/man/dapc_plot.html.
  
  data <- read.table("genotypes_admixture.3.Q", header = FALSE, sep = "\t")
  
#  genind <- df2genind(data); \
  genind.dapc <- dapc(genind, n.pca = 20); \
  pdf("dapc_plot.pdf", width = 8, height = 8); \
  scatter(genind.dapc, posi.da = TRUE, cell = 0.8); \
  dev.off()
  
  
  
  'library(adegenet); library(ggplot2); \
  data <- read.table("genotypes_admixture.3.Q", header = FALSE); \
  genind <- df2genind(data); \
  genind.dapc <- dapc(genind, n.pca = 20); \
  pdf("dapc_plot.pdf", width = 8, height = 8); \
  scatter(genind.dapc, posi.da = TRUE, cell = 0.8); \
  dev.off()'
  
  
  # View the first few rows of the Q-matrix
head(qmatrix)

# Calculate the distance matrix
dist_matrix <- dist(qmatrix)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")  # Using Ward's method for clustering

# Plot the dendrogram with bolder lines
plot(as.dendrogram(hc), main = "Population Structure Dendrogram", 
     xlab = "Individuals", ylab = "Height", lwd = 2, cex.lab = 1.5, cex.main = 1.5)

# Cut the tree into clusters (e.g., 3 clusters)
clusters <- cutree(hc, k = 3)

# Assign colors to the clusters
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)

# Plot the colored dendrogram with bolder lines
plot(dend, main = "Colored Population Structure Dendrogram", 
     xlab = "Individuals", ylab = "Height", lwd = 2, cex.lab = 1.5, cex.main = 1.5)

# Plot the colored dendrogram again, with bold adjustments
plot(dend, main = "Population Structure Dendrogram", 
     xlab = "Individuals", ylab = "Height", lwd = 2, cex.lab = 1.5, cex.main = 1.5)



# Plot the colored dendrogram with bold and bright colors
plot(dend, main = "Population Structure Dendrogram", 
     xlab = "Individuals", ylab = "Height", 
     lwd = 4,  # Increase line width for bolder branches
     cex.lab = 2, cex.main = 2)

# Assign bright colors to clusters
dend <- color_branches(dend, k = 3, col = c("red", "blue", "green"))  # Bright colors for clusters

# Plot again with bolder, colored branches
plot(dend, main = "Population Structure Dendrogram", 
     xlab = "Individuals", ylab = "Height", 
     lwd = 3,  # Bold line width
     cex.lab = 1.5, cex.main = 1.5)


# Load required packages
library(ggplot2)

# Read in the Q matrix
q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)

# Assign sample names to rows
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Sort individuals by similarity in their subgroup proportions
q_matrix_sorted <- q_matrix[order(rowSums(q_matrix^2)),]

# Plot the barplot with sorted individuals
barplot(t(q_matrix_sorted), col = rainbow(3), border = NA, names.arg = rownames(q_matrix_sorted), las = 2, main = "Population Structure (Sorted)")
legend("topright", legend = paste("Population", 1:3), fill = rainbow(3), border = NA)

# Save the plot
ggsave("admixture_plot_sorted.png", width = 10, height = 6, units = "in")











# Example R script to plot ADMIXTURE results with aligned barplot and dendrogram

# Load necessary libraries
library(ggplot2)
library(dendextend)

# Set working directory (adjust the path accordingly)
setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/ADMIXTURE MID MARKERS/combined")

# Load the Q-matrix file
q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)

# Assign sample names to rows
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Calculate the distance matrix based on the Q-matrix
dist_matrix <- dist(q_matrix)

# Perform hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Reorder the Q-matrix according to the hierarchical clustering order
ordered_q_matrix <- q_matrix[hc$order, ]

# Plot the barplot for population structure with reordered individuals
barplot(t(ordered_q_matrix), col = rainbow(3), border = NA, names.arg = rownames(ordered_q_matrix), las = 2, main = "Population Structure (Ordered by Clustering)")
barplot(t(ordered_q_matrix), col = rainbow(3), border = NA, names.arg = rownames(ordered_q_matrix), las = 2, main = "Population Structure (Ordered by Clustering)")
legend("topright", legend = paste("Population", 1:3), fill = rainbow(3), border = NA)

# Save the population structure plot
ggsave("ordered_admixture_plot.png", width = 10, height = 6, units = "in")

# Create and plot the dendrogram with colored branches
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 3)  # Color branches by cluster

# Plot the colored dendrogram with reordered individuals
plot(dend, main = "Population Structure Dendrogram", xlab = "Individuals", ylab = "Height")

# Save the dendrogram plot
ggsave("dendrogram_plot.png", width = 10, height = 6, units = "in")












# Example R script to plot ADMIXTURE results with aligned barplot and dendrogram

# Load necessary libraries
library(ggplot2)
library(dendextend)

# Set working directory (adjust the path accordingly)
setwd("/Users/abiodunolayinka/Documents/Abiodun Olayinka/DATA ANALYSIS/ADMIXTURE MID MARKERS/combined")

# Load the Q-matrix file
q_matrix <- read.table("MIDMARKERS_filtered.3.Q", header = FALSE)

# Assign sample names to rows
rownames(q_matrix) <- paste("Individual", 1:nrow(q_matrix))

# Calculate the distance matrix based on the Q-matrix
dist_matrix <- dist(q_matrix)

# Perform hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Reorder the Q-matrix according to the hierarchical clustering order
ordered_q_matrix <- q_matrix[hc$order, ]

# Adjust margins to leave more space for the title (top margin is the first number in par(mar))
par(mar = c(5, 4, 6, 2))  # Bottom, left, top, and right margins

# Plot the barplot for population structure with reordered individuals and a larger title
barplot(t(ordered_q_matrix), col = rainbow(3), border = NA, names.arg = rownames(ordered_q_matrix), las = 2, 
        main = "Population Structure", cex.main = 1.5)  # cex.main enlarges the title

# Restore default margins
par(mar = c(5, 4, 4, 2))  # Reset to typical margins


