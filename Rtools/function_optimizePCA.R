## === Optimize PC inclusion to Remove Noise Variance and Visualize Data in Reduced Dimensional Space from PCA === ##
# Function performs PCA on a given data object and provides metrics and visualization to aid in the capture of important variance in the reduced dimensional space
# Functionality includes analyzing cumulative variance, evaluation of variance distribution across important PCs and visualization of score plots for the top 3 PC 
# arguments:
  # data_object = matrix-like data structure with features in columns and species in rows
  # feature_labels = character or numeric vector of length equal to columns or rows of data_object with feature of interst labels labels
  # usage = character vector to define how to handle the data object; "features" (analyze by features) or "species" (analyze by species); default set to "features"
  # return_data = logical value to determine if the pca_object is returned; default set to TRUE
  # plot_data = logical value to determine if the PC score plots are visualized; default set to TRUE
  # center_data = logical vector passed to the prcomp() function to center the data; default set to TRUE
  # scale_data = logical vector passed to the prcomp() function to scale the data; default set to FALSE
  # total_variance = numeric value between 0 and 1 representing the percentage of total variance to capture and relevant to the biology of interest
# dependencies:
  # stats

optimizePCA = function (data_object, feature_labels, usage = "features", return_data = TRUE, plot_data = TRUE, center_data = TRUE, scale_data = FALSE, total_variance = 0.8) {
  # flow control to change if PCA is run against features or species of original data (target must be in rows)
  if (usage == "features") {
    hold_data = t(data_object)
  } else if (usage == "species") {
    hold_data = data_object
  }
  
  pca_object = prcomp(x = hold_data, center = center_data, scale. = scale_data) # PCA object
  target_PCs = filterPC(pca_object = pca_object, variance_percent = total_variance) # find total PC needed to include the user defined total variance of the original data
  stats::screeplot(pca_object, type = "lines", npcs = target_PCs, main = "Contribution to Variance by Principal Components", pch = 16) # screeplot to visualize the variance distribution of included PCs
  title(xlab = "Principal Components")
  
  # flow control for visualizing the top 3 PCs 2D plots
  if (plot_data) {
    plot_data = as.data.frame(pca_object$x[,1:3])
    colnames(plot_data) = c("PC1", "PC2", "PC3")
    plot_data$features = feature_labels
     if (usage == "features") {
       set.seed(1)
       plot_data$feature_colors = colorCode(feature_data = feature_labels, color_replace = FALSE)
       p1 = ggplot(data = plot_data, aes(x = PC1, y = PC2, color = factor(features))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "top",legend.title = element_blank(),legend.text = element_text(size = 8))
       p2 = ggplot(data = plot_data, aes(x = PC1, y = PC3, color = factor(features))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "top",legend.title = element_blank(),legend.text = element_text(size = 8))
       p3 = ggplot(data = plot_data, aes(x = PC2, y = PC3, color = factor(features))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "top",legend.title = element_blank(),legend.text = element_text(size = 8))
       gridExtra::grid.arrange(grobs = list(p1,p2,p3), nrow = 1, ncol = 3, main = "Plots of Top 3 Principal Components")
     } else if (usage == "species") {
       species_colors = colorCode(feature_data = feature_labels, color_replace = TRUE)
       p1 = ggplot(data = plot_data, aes(x = PC1, y = PC2, color = species_colors)) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
       p2 = ggplot(data = plot_data, aes(x = PC1, y = PC3, color = species_colors)) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
       p3 = ggplot(data = plot_data, aes(x = PC2, y = PC3, color = species_colors)) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
       gridExtra::grid.arrange(grobs = list(p1,p2,p3), nrow = 1, ncol = 3, main = "Plots of Top 3 Principal Components")
     }
  }
  
  # flow control to return PCA object
  if (return_data) {
    return(pca_object)
  }
}
