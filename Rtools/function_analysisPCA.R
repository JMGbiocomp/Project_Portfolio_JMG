## === Comprehensive PCA for Exploratory Analysis === ##
# Function performs a PCA to explore clustering of a given data feature and the major species contributors to the covariance
# Functionality includes visualization of the PC score and loading plots of the top 3 PCs
# A threshold for the bidirectional magnitude of the PC loadings for the first three PCs determines the major contributors
# Input data can be scaled when performing the PCA
# Arguments:
  # data_object = matrix-like data structure with data features in columns and species in rows including columns and row names
  # scale_data = logical value to determine if the data is scaled for constructing the PCA object (R stats prcomp() function)
  # plot_data = logical value to determine if PC score and loading plots are generated 
  # data_return = logical value to determines if the pca object is returned
  # load_threshold = numeric value between 0 and 1 as a cut off for magnitudinal direction of pca loadings 
# Dependencies:
  # stats, ggplot2, gridExtras

analysisPCA = function (data_object, feature_labels, center_data = TRUE, scale_data = FALSE, plot_data = TRUE, data_return = FALSE, load_threshold = 0.5, top_result = 10, PC_error = 10) {
  # Flags and errors
  
  # perform PCA
  pca_object = stats::prcomp(t(data_object), center = center_data, scale. = scale_data)
  
  if (plot_data == TRUE) {
    # build data structures for PC scores and loading plots
    pca_scores = as.data.frame(pca_object$x[,1:3]) # top three PC only
    colnames(pca_scores) = c("PC1", "PC2", "PC3")
    rownames(pca_scores) = colnames(data_object)
    pca_scores$feature = feature_labels
    
    pca_loading = as.data.frame(pca_object$rotation[,1:3]) # top 3 PC only
    colnames(pca_loading) = c("PC1", "PC2", "PC3")
    rownames(pca_loading) = rownames(data_object)
    pca_loading$species = rownames(data_object)
    
    # score plots
    s1 = ggplot(data = pca_scores, aes(x = PC1, y = PC2, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    s2 = ggplot(data = pca_scores, aes(x = PC1, y = PC3, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    s3 = ggplot(data = pca_scores, aes(x = PC2, y = PC3, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    
    # loading plots
    l1 = ggplot(data = pca_loading, aes(x = PC1, y = PC2)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC2) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC2) > load_threshold), aes(label = species, vjust = -1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    l2 = ggplot(data = pca_loading, aes(x = PC1, y = PC3)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC3) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC3) > load_threshold), aes(label = species, vjust = -1))  + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    l3 = ggplot(data = pca_loading, aes(x = PC2, y = PC3)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC2) > load_threshold | abs(PC3) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC2) > load_threshold | abs(PC3) > load_threshold), aes(label = species, vjust = -1))  + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    
    # flow control to manage data centering and scaling for analyzing global data structure 
    if (center_data == FALSE) {
      hold_pca = stats::prcomp(t(data_object), center == TRUE, scale. = scale_data)
      if (scale_data == TRUE) {
        hold_data = scale(t(data_object), center = TRUE, scale = TRUE)
      } else {
        hold_data = scale(t(data_object), center = TRUE, scale = FALSE)
      }
    } else {
      hold_pca = pca_object
      if (scale_data == TRUE) {
        hold_data = scale(t(data_object), center = TRUE, scale = TRUE)
      } else {
        hold_data = scale(t(data_object), center = TRUE, scale = FALSE)
      }
    }
    # Covariance distribution (screeplot) and global anomaly detection (reconstruction error)
    reconstruct_data = t(t(hold_pca$x[, 1:PC_error] %*% t(hold_pca$rotation[, 1:PC_error])) * hold_pca$scale + hold_pca$center)
    errors = (hold_data - reconstruct_data)^2
    average_error = c(rowMeans(errors))
    label_criteria = which(average_error > mean(average_error)+sd(average_error)*3)
    
    stats::screeplot(pca_object, type = "lines", npcs = PC_error, main = "Weight of Top PC Contribution to Variance", pch = 16)
    title(xlab = "Principal Components", ylab = "Captured Variance")
    barplot(average_error, main = "Reconstruction Error by Feature", log = "y", las = 2, xaxt = "n", ylim = c(mean(average_error) - sd(average_error)*6, mean(average_error) + sd(average_error)*6), col = "steelblue") 
    axis(side = 1, at = label_criteria, labels = colnames(data_object)[label_criteria], cex = 0.5)
    title(ylab = "Root Mean Squared Error (RMSE)")
    abline(h = mean(average_error), col ="green", lty = "dashed")
    abline(h = mean(average_error) + sd(average_error)*2, col = "blue", lty = "dashed")
    abline(h = mean(average_error) + sd(average_error)*6, col = "red", lty = "dashed")
    
    # Local outlier detection using local outlier factor (LOF)
    analysisLOF(pca_object$x, plot_return = TRUE)
    
    # Local outlier detection (LOD) using mahalanobis distance 
    LOD1 = analysisLOD(data_object = pca_object$x[,1:2], feature_labels = feature_labels, data_type = "pca", d_names = c("PC1", "PC2"))
    LOD2 = analysisLOD(data_object = pca_object$x[,c(1,2)], feature_labels = feature_labels, data_type = "pca", d_names = c("PC1", "PC3"))
    LOD3 = analysisLOD(data_object = pca_object$x[,2:3], feature_labels = feature_labels, data_type = "pca", d_names = c("PC2", "PC3"))
    
    p1 = ggplot(data = LOD1, aes(x = PC1, y = PC2, color = factor(feature))) + geom_point(size = distance) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    p2 = ggplot(data = LOD2, aes(x = PC1, y = PC3, color = factor(feature))) + geom_point(size = distance) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    p3 = ggplot(data = LOD3, aes(x = PC2, y = PC3, color = factor(feature))) + geom_point(size = distance) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    
    gridExtra::grid.arrange(grobs = list(p1,p2,p3), nrow = 1, ncol = 3, main = "Local Outlier Detection")
    gridExtra::grid.arrange(grobs = list(s1,s2,s3,l1,l2,l3), nrow = 2, ncol = 3, main = "PCA Scores and Loadings")
    
    top_species = c()
    for (i in 1:3) {
      temp = pca_loading[order(abs(pca_loading[,i]), decreasing = TRUE),]
      top_species = c(top_species, rownames(temp)[1:top_result])
    }
    print("top species contributors:")
    print(unique(top_species))
  }
  
  if (data_return) {
    return(pca_object)
  } else {
    message("data_return arg default: no data returned")
  }
}
                                                                                     