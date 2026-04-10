##
#
#
#
#
# argument:
  # data_object
  # feature_labels
  # transpose_data
  # dist_method = one of several different distance calculaiton methods (euclidean, manhattan, pearson, spearman); default = euclidean; 
  # linkage_method = one of several different linkage methods ("ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid");
  # dend_plot
  # object_return
  #
# dependencies:

hclustObject = function (data_object, feature_labels, transpose_data = FALSE, dist_method = "euclidean", linkage_method, dend_plot = FALSE, object_return = "hclust") {
  
  if (transpose_data == TRUE) {
    data_object == t(data_object)
  }
  
  if (dist_method == "manhattan") {
    h_object = hclust(d = dist(data_object, method = dist_method), method = linkage_method)
  } else if (dist_method == "pearson") {
    h_object = hclust(d = as.dist(1-stats::cor(data_object)), method = linkage_method)
  } else if (dist_method == "spearman") {
    h_object = hclust(d = as.dist(1-stats::cor(data_object, method = dist_method)), method = linkage_method)
  } else if (dist_method == "euclidean") {
    h_object = hclust(d = dist(data_object, method = dist_method), method = linkage_method)
  }
  
  if (dend_plot) {
    dend = as.dendrogram(h_object)
    labels(dend) = feature_labels # label dendrogram leaves
    plot(dend, main = linkage_method)
  }
  
  if (object_return == "dendrogram") {
    dend = as.dendrogram(h_object)
    labels(dend) = feature_labels # label dendrogram leaves
    return(dend)
  } else if (object_return == "hclust") {
    return(h_object)
  }
}



x = hclustObject(data_object = pca_genes$x, feature_labels = rownames(RNAseqCounts_topVar), transpose_data = FALSE, dist_method = "euclidean", linkage_method = "average", dend_plot = FALSE, object_return = "hclust")
hclustObject(data_object = hold_data, feature_labels = features, transpose_data = FALSE, dist_method = dist_method, linkage_method = algorithm_method, dend_plot = FALSE, object_return = "hclust")
dim(x)
y = cutree(x, k = 3)
cutree(cluster_object, k = cluster_count)
unique(y)
y
length(y)
