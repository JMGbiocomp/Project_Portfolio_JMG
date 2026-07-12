## === Plotting CLuster Assignments to the Data in Two-Dimensional Space === ##
# Function generates ggplot2 visualizations of the data against a provided set of feature labels and cluster assignments for comparison
# 
#
#
#
# Arguments:


plotCluster = function (data_object = NULL, feature_labels = NULL, cluster_labels = NULL, usage = "features", verbose = FALSE) {
  # Messages, Warnings, and Errors
  if (is.null(data_object)) {stop("Must provide a matrix-like data object with feature of interest in rows.")}
  if (is.null(feature_labels)) {stop("Must provide a numeric or character vector of feature labels for the feature of interest.")}
  if (length(feature_labels) != dim(data_object)[1]) {
    print(dim(data_object))
    print(length(feature_labels))
    stop("Feature labels length does not equal the number of features of interest in the provided data_object.")
  } else (
    if (verbose) {message("Feature labels variable passes criteria")}
  )
  if (is.null(cluster_labels)) {stop("Must provide a numeric or character vector of cluster assignments for the feature of interest.")}
  if (length(cluster_labels) != dim(data_object)[1]) {
    print(dim(data_object))
    print(length(cluster_labels))
    stop("Cluster assignment length does not equal the number of features of interest in the provided data_object.")
  }else (
    if (verbose) {message("Cluster assignments variable passes criteria")}
  )
  
  # 
  
}