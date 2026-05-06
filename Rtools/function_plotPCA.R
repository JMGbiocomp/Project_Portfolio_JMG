### === Visualizations of Targeted PCs === ###
# Function generates a 2D plot of a two target PCs for analysis against a user defined feature variable
# Recommended to investigate at least one dimension of a PCA (PCs) beyond the top 3
# arguments:
  # pca_object = a PCA object from the stats package prcomp() function
  # feature_labels = character or numeric vector of length equal to the length of the feature the prcomp() was applied to
  # axes = a numeric vector of length 2 for the PCs under investigation
  # axes_labels = a character vector of length 2 for the names of the PCs under investigation
# dependencies
  # stats

plotPCA = function (pca_object, feature_labels, axes = c(1,2), axes_labels) {
  dim1 = c(pca_object$x[,axes[1]])
  dim2 = c(pca_object$x[,axes[2]])
  
  feature_colors = codeColor(feature_data = feature_labels)
  plot(x = dim1, y = dim2, xlab = axes_labels[1], ylab = axes_labels[2], col = feature_colors, main = "2D Check Plot for PC Scores")
}

