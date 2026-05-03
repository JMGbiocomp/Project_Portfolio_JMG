## === Comprehensive PCA for Exploratory Analysis === ##
# Function performs a PCA to explore clustering of a given data feature and the major species contributors to the covariance
# Functionality includes visualization of the PC score and loading plots of the top 3 PCs
# A threshold for the bidirectional magnitude of the PC loadings for the first three PCs determines the major contributors
# Input data can be centered and scaled when performing the PCA
# Arguments:
  # data_object = matrix-like data structure with data features in columns and species in rows including columns and row names
  # feature_labels = character or numeric vector of length equal to columns of data_object containing the labels of a feature of interest for investigation
  # center_data = logical value to determine if the data is centered for constructing the PCA object (R stats prcomp() function); default set to TRUE
  # scale_data = logical value to determine if the data is scaled for constructing the PCA object (R stats prcomp() function); default set to FALSE
  # plot_data = logical value to determine if PC score and loading plots are generated; default set to TRUE
  # data_return = logical value to determines if the pca object is returned; default set to FALSE
  # load_threshold = numeric value between 0 and 1 as a cut off for magnitudinal direction of pca loadings; default set to 0.5 (recommend evaluating loading plots prior to adjusting)
  # noise_filter = numeric value between 0 and 1 as the percentage of variance to capture from included PCs in analysis and filter the remaining variance as noise; default set to 0.8 (recommended 0.7-0.9)
# Dependencies:  
  # stats, ggplot2, gridExtras

analysisPCA = function (data_object, feature_labels, center_data = TRUE, scale_data = FALSE, plot_data = TRUE, data_return = FALSE, load_threshold = 0.5, noise_filter = 0.8) {
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
    s1 = ggplot(data = pca_scores, aes(x = PC1, y = PC2, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
    s2 = ggplot(data = pca_scores, aes(x = PC1, y = PC3, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
    s3 = ggplot(data = pca_scores, aes(x = PC2, y = PC3, color = factor(feature))) + geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
    
    # loading plots
    l1 = ggplot(data = pca_loading, aes(x = PC1, y = PC2)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC2) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC2) > load_threshold), aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    l2 = ggplot(data = pca_loading, aes(x = PC1, y = PC3)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC3) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC1) > load_threshold | abs(PC3) > load_threshold), aes(label = species, vjust = -1), size = 2)  + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    l3 = ggplot(data = pca_loading, aes(x = PC2, y = PC3)) + geom_point(color = "lightgrey") + geom_point(data = subset(pca_loading, abs(PC2) > load_threshold | abs(PC3) > load_threshold), color = "red") + geom_text(data = subset(pca_loading, abs(PC2) > load_threshold | abs(PC3) > load_threshold), aes(label = species, vjust = -1), size = 2)  + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    
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
    # Variance distribution analysis across PCs
    par(mfrow = c(1,1))
    target_PCs = filterPC(pca_object, variance_percent = noise_filter) # Identifying the PC count before noise is introduced
    stats::screeplot(pca_object, type = "lines", npcs = target_PCs, main = "Contribution to Variance by Principal Components", pch = 16) # analyzing variance distribution across relevent PCs
    title(xlab = "Principal Components")
    
    # Global anomaly detection
    index_RE = analysisRE(data_object = hold_data, pca_object = hold_pca, npc = target_PCs)
    message("Gloabl anomalies detected by calculating reconstruction error:")
    print(index_RE)
    
    # Local outlier detection (LOD) using local outlier factor (LOF)
    index_LOF = analysisLOF(pca_object$x[,1:target_PCs], plot_return = TRUE, data_return = "index")
    message("Local outliers to consider by LOF score:")
    print(index_LOF)
    
    # Local outlier detection (LOD) using mahalanobis distance 
    LOD1 = analysisMahalabonis(data_object = pca_object$x[,1:2], feature_labels = feature_labels)
    LOD2 = analysisMahalabonis(data_object = pca_object$x[,c(1,3)], feature_labels = feature_labels)
    LOD3 = analysisMahalabonis(data_object = pca_object$x[,2:3], feature_labels = feature_labels)
    
    message("Local outliers to consider by mahalanobis distance and a chi-squared threshold:")
    if (length(LOD1) < 1) {
      message("none detected using PC1 and PC2 as dimensions")
    } else {
      print(LOD1)
    }
    if (length(LOD1) < 1) {
      message("none detected using PC1 and PC3 as dimensions")
    } else {
      print(LOD2)
    }
    if (length(LOD1) < 1) {
      message("none detected using PC2 and PC3 as dimensions")
    } else {
      print(LOD3)
    }
    
    par(mfrow = c(1,1))
    temp = subset(pca_loading, abs(pca_loading[,1]) > load_threshold | abs(pca_loading[,2]) > load_threshold | abs(pca_loading[,3]) > load_threshold)
    analysisLoadings(pca_object = pca_object, species_labels = rownames(temp), npcs = target_PCs)
    par(mfrow = c(1,1))
    
    outliers = unique(c(index_LOF, index_RE, LOD1, LOD2, LOD3)) # indexes of global and local outliers detected by RE and LOF
    outlier_data = pca_object$x[outliers,] # pca score data filtered by outliers
    
    o1 = ggplot(data = pca_scores, aes(x = PC1, y = PC2)) + geom_point(data = pca_scores, color = "grey") + geom_point(data = outlier_data, color = "black") + geom_text(data = outlier_data, aes(label = outliers, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    o2 = ggplot(data = pca_scores, aes(x = PC1, y = PC3)) + geom_point(data = pca_scores, color = "grey") + geom_point(data = outlier_data, color = "black") + geom_text(data = outlier_data, aes(label = outliers, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    o3 = ggplot(data = pca_scores, aes(x = PC2, y = PC3)) + geom_point(data = pca_scores, color = "grey") + geom_point(data = outlier_data, color = "black") + geom_text(data = outlier_data, aes(label = outliers, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    
    # Analytical Plots
    gridExtra::grid.arrange(grobs = list(s1,s2,s3,o1,o2,o3), nrow = 2, ncol = 3, top = "Mapping Outliers by RE, LOF and Mahalanobis Distance") # Evaluating outliers against data in reduced space
    gridExtra::grid.arrange(grobs = list(s1,s2,s3,l1,l2,l3), nrow = 2, ncol = 3, top = "PCA Scores and Loadings") # Viewing species that meet load threshold for 2D score plots
  }                                                                                                                                                                                                                                                           
  
  if (data_return & plot_data) {
    return(rownames(temp))
  } else if (data_return) {
    return(pca_object)
  } else {
    message("data_return arg default: no data returned")
  }
}
                                                                                     