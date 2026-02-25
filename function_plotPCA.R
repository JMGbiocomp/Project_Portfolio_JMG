### === Visualizations of principal components (highest covariance) within a data set === ###
# Function takes a prcomp object (PCA) and plots three 2D plots to visualize the top three principal components and/or a 3D plot of the top three all together 
# Ideal for exploratory (unsupervised) analysis of covariances and its possible alignment with a data feature(e.g. treatment or phenotype)
# arguments:
  # count_data = count data for PCA; functionality extends to non-count data for other variable/distribution types
  # feature_labels = character vector of the categories, treatments, or phenotype corresponding to each sample under potential investigation
  # plot2D = boulean vlaue to determine if 2D plots will be generated; default = TRUE
  # plot3D = boulean value to determine if a 3D plot will be generated; default = FALSE
  # usage = single element character vector to frame the PCA by either sample scores or species loading; default is set to "sample" but can also be set to "species"

library(rgl); library(BiocGenerics)
plotPCA = function (count_data = NA, feature_labels = NA, plot2D = TRUE, plot3D = FALSE, usage = "sample", return = FALSE) {
  # errors and flags
  if (is.na(count_data)) {stop("requires a count matrix or equivalaent")}
  if (is.na(feature_labels)) {stop("must provide a character vector of sample features under investigation for the feature_labels argument")}
  if (length(c(t(pca_object$x[,1]))) != length(feature_labels)) {stop("feature_labels argument vector must be the same number of samples used for the prcomp object")}
  
  # define the PCA framework
  if (usage =="sample") {
    pca_object = prcomp(t(count_data), scale = FALSE)
  } else if (usage == "species") {
    pca_object = prcomp(count_data, scale = FALSE)
  }
  
  # extract the first three principal components
  X = pca_object$x[,1] # PC1
  Y = pca_object$x[,2] # PC2
  Z = pca_object$x[,3] # PC3
  
  if (plot2D == TRUE) {
    groups = unique(feature_labels) # identify only the unique labels
    colors = sample(x = colors(distinct = TRUE), size = length(feature_labels)) # color code vector to designate feature labels by color
    point_colors = colors[match(feature_labels, groups)] # generates a color code vector corresponding to each sample's feature
    
    # create 2D plots fro PCA
    par(mfrow = c(1,3)) # formats the plot into a single frame
    # PC1 vs PC2
    plot(X, Y,
         col = point_colors,
         pch = 16, # solid color circle shape for point
         main = "PCA: PC2 vs PC1", # title
         xlab = "PC1",
         ylab = "PC2",
         cex = 1.5
         )
    legend("topright",
           col = colors,
           pch = 16, # solid color circle shape for point
           cex = 1, # legend text size
           title = "Principal Components",
           bty = "o" # draws a box around the legend
    )
    # PC1 vs PC3
    plot(X, Z,
         col = point_colors,
         pch = 16, # solid color circle shape for point
         main = "PCA: PC3 vs PC1", # title
         xlab = "PC1",
         ylab = "PC3",
         cex = 1.5
    )
    legend("topright",
           col = colors,
           pch = 16, # solid color circle shape for point
           cex = 1, # legend text size
           title = "Principal Components",
           bty = "o" # draws a box around the legend
    )
    # PC2 vs PC3
    plot(Y, Z,
         col = point_colors,
         pch = 16, # solid color circle shape for point
         main = "PCA: PC3 vs PC2", # title
         xlab = "PC2",
         ylab = "PC3",
         cex = 1.5
    )
    legend("topright",
           col = colors,
           pch = 16, # solid color circle shape for point
           cex = 1, # legend text size
           title = "Principal Components",
           bty = "o" # draws a box around the legend
    )
  }
  
  if (plot3D == TRUE) {
    par(mfrow = c(1,1))
    colors <- rainbow(nlevels(feature_labels)) # color codes features
    col_assigned <- colors[as.numeric(feature_labels)] # assigned colors codes to all samples
    # 3D plot
    plot3d(X, Y, Z,
           col = col_assigned, type = 's', size = 1)
    # Add legend
    legend3d("topright", legend = levels(feature_labels), pch = 16, col = colors)
  }
  if (return == TRUE) {return(pca_object)}
}