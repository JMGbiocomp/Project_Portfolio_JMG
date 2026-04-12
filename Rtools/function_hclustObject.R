## === Generate a hierarchical clustering object for analsyis === ##
# Function builds a hierarchical clustering object to the specified distacne calculation and linkage method
# Improved functionality over previous function 
#
#
# argument:
  # data_object
  # feature_labels
  # transpose_data
  # dist_method = one of several different distance calculation methods (euclidean, manhattan, pearson, spearman); default = euclidean; 
  # linkage_method = one of several different linkage methods ("ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid");
  # dend_plot
  # object_return
  #
# dependencies:

hclustObject = function (data_object, feature_labels, transpose_data = "features", dist_method = "euclidean", linkage_method, dend_plot = FALSE, object_return = "hclust", verbose = FALSE) {
  
  if (dist_method == "manhattan") {
    if (transpose_data == "features") {
      data_object = t(data_object)
    }
    h_object = hclust(d = dist(data_object, method = dist_method), method = linkage_method)
  } else if (dist_method == "pearson") {
    if (transpose_data == "species") {
      data_object = t(data_object)
    }
    h_object = hclust(d = as.dist(1-stats::cor(data_object)), method = linkage_method)
  } else if (dist_method == "spearman") {
    if (transpose_data == "species") {
      data_object = t(data_object)
    }
    h_object = hclust(d = as.dist(1-stats::cor(data_object, method = dist_method)), method = linkage_method)
  } else if (dist_method == "euclidean") {
    if (transpose_data == "features") {
      data_object = t(data_object)
    }
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
    if (verbose) {
      print("dendrogram compete")
    }
    return(dend)
  } else if (object_return == "hclust") {
    if (verbose) {
      print("hclust object complete")
    }
    return(h_object)
  }
}
