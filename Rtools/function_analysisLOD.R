## === Local Outlier Detection by a Defined Groupings/Clusters === ##
# Function utilizes the mahalanobis distance from a calculated center based on defined data groups or clusters to detect local outliers
# Groups can be defined by data feature or clustering results
# Input data must be two dimensional either by PCA, tSNE, or UMAP dimensionality reduction techniques
# arguments:
  # data_object = matrix-like data structure with feature of interest in rows and dimensional coordinates in columns; Output from dimensionality reduction techniques: PCA ($x), tSNE ($Y), and UMAP ($layout) accepted.
  # feature_labels = groupings or clustering of the feature of interest to frame the local outlier algorithm against to find data points dissimilar to others of its saem group/cluster
  # data_type = dimensionality reduction technique used to create the data object: c("pca", "tsne", "umap")
  # d_names = character vector for the dimension names for a data_object of the "pca" data_type; used to correctly label whihc principal components are being used for the execution
# dependencies:
  # stats

analysisLOD = function (data_object, feature_labels, sd_count = 3) {
  par(mfrow = c(1,3))
  hold_data = as.data.frame(data_object)
  hold_data$features = feature_labels
  hold_data$sample = rownames(hold_data)
  hold_data$Index = 1:dim(hold_data)[1]
  hold_data$distance = c(0)
  groups = unique(feature_labels)
  
  index_outliers = c()
  for (i in 1:length(groups)) {
    feature_data = hold_data[hold_data$feature == groups[i],]
    feature_data$distance = mahalanobis(x = feature_data[,1:2], colMeans(feature_data[,1:2]), cov = cov(feature_data[,1:2])) 
    target_data = subset(feature_data, feature_data$distance > mean(feature_data$distance) + sd_count*sd(feature_data$distance))
    index_outliers = c(index_outliers, target_data$Index)
    label_criteria = which(feature_data$distance > mean(feature_data$distance)+2*sd(feature_data$distance))
    
    plot(x = feature_data$Index, y = feature_data$distance, main = paste("Detection of Local Outliers (LOD):",groups[i] ), ylim = c(0,mean(feature_data$distance) + sd(feature_data$distance)*10), ylab = "Mahalanobis Distance", xlab = "Index", col = "black", cex = 0.8) 
    text(x = target_data$Index, y = target_data$distance, labels = label_criteria, pos = 3, cex = 0.8)
    abline(h = median(feature_data$distance), col ="green", lty = "dashed")
    abline(h = mean(feature_data$distance) + sd(feature_data$distance)*2, col = "blue", lty = "dashed")
    abline(h = mean(feature_data$distance) + sd(feature_data$distance)*6, col = "red", lty = "dashed")
    
    index = 1
    for (r in 1:dim(hold_data)[1]) {
      if (hold_data[r,"features"] == groups[i]) {
        hold_data[r, "distance"] = feature_data[index,"distance"]
        index = index + 1
      } 
    }
  }
  par(mfrow = c(1,1))
  return(index_outliers)
}