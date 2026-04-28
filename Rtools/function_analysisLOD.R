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

analysisLOD = function (data_object, feature_labels, data_type, d_names) {
  hold_data = as.data.frame(data_object)
  if (data_type == "pca") {
    colnames(hold_data) = d_names
  } else if (data_type == "tsne") {
    colnames(hold_data) = c("tSNE1","tSNE2")
  } else if (data_type == "umap") {
    colnames(hold_data) = c("UMAP1", "UMAP2")
  }
  hold_data$feature = c(feature_labels)
  hold_data$code = colorCode(feature_labels, color_replace = FALSE)
  hold$data$sample = rownames(hold_data)
  hold_data$distance = c(0)
  groups = unique(feature_labels)
  
  for (i in 1:length(groups)) {
    feature_data = hold_data[hold_data$feature == groups[i],]
    feature_data$distance = mahalanobis(x = feature_data[,1:2], colMeans(feature_data[,1:2]), cov = cov(feature_data[,1:2]))
    index = 1
    for (r in 1:dim(hold_data)[1]) {
      if (hold_data[r,"feature"] == groups[i]) {
        hold_data[r, "distance"] = feature_data[index,"distance"]
        index = index + 1
      } 
    }
  }
  return(hold_data)
}
