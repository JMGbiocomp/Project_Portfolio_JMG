### === 2D visualizations of principal components (highest covariance) within a data set using ggplot2 === ###
# Function takes a prcomp object (PCA) and plots three 2D plots to visualize the top three principal components and/or a 3D plot of the top three all together 
# Ideal for exploratory (unsupervised) analysis of covariances and its possible alignment with a data feature(e.g. treatment or phenotype)
# arguments:
  # count_data = count data for PCA; functionality extends to non-count data for other variable/distribution types
  # feature_labels = character vector of the categories, treatments, or phenotype corresponding to each sample under potential investigation
  # usage = single element character vector to frame the PCA by either sample scores or species loading; default is set to "sample" but can also be set to "species"
  # return = logical value to determine if the prcomp object (PCA) is returned
# dependencies
  # ggplot2
  # gridExtras

ggplotPCA = function (count_data = NA, feature_labels = NA, usage = "sample", return = FALSE) {
  # define the PCA framework
  if (usage =="sample") {
    pca_object = prcomp(x = (t(count_data)), scale = FALSE)
  } else if (usage == "species") {
    pca_object = prcomp(count_data, scale = FALSE)
  }
  
  if (length(c(t(pca_object$x[,1]))) != length(feature_labels)) {stop("feature_labels argument vector must be the same number of samples used for the prcomp object")}
  
  groups = unique(feature_labels) # identify only the unique labels
  colors = sample(x = colors(distinct = TRUE), size = length(groups)) # color code vector to designate feature labels by color
  point_colors = colors[match(feature_labels, groups)] # generates a color code vector corresponding to each sample's feature
    
  # create 2D plots for PCA
  # PC1 vs PC2
  p1 = ggplot(data = pca_object$x, aes(x = PC1, y = PC2, color = feature_labels))+ geom_point(size = 1.5) + scale_color_manual(values = colors)+ labs(x = "PC1", y = "PC2")+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) # Optional: adds axis lines back)
  # PC1 vs PC3
  p2 = ggplot(data = pca_object$x, aes(x = PC1, y = PC3, color = feature_labels)) + geom_point(size = 1.5) + scale_color_manual(values = colors) + labs(x = "PC1", y = "PC3") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) # Optional: adds axis lines back)
  # PC2 vs PC3
  p3 = ggplot(data = pca_object$x, aes(x = PC2, y = PC3, color = feature_labels)) + geom_point(size = 1.5) + scale_color_manual(values = colors) + labs(x = "PC2", y = "PC3")+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) # Optional: adds axis lines back)
  # grid
  grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, top = "Principal Component Analysis")
  
  if (return == TRUE) {
    return(pca_object)
  }
}
