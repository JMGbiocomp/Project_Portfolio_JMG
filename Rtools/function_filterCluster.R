## === Filter Features or Species by Clustering Output === ##
# Function filters the data set to find features or species corresponding to a set of one or more cluster IDs from the results an unsupervised learning method (clustering)
#
# argument:
  # data_object = a matrix-like data structure with features in columns and species in rows
  # cluster_ID = a numeric vector of the clustering results of an applied clustering algorithm relevant to the data_object
  # target_clusters = numeric vector containing target cluster assignments to filter the data with to extract feature or species labels
  # usage = character vector indicating if the data_object's features or species are of interest for filtering
# dependencies:
  # 

filterCluster = function (data_object = NULL, cluster_ID = NULL, target_clusters = NULL, usage = "species") {
  # errors, warnings, and flags
  if (is.null(data_object)) {stop("Must provide a data_object with species in the rows and features in the columns to match with cluster_ID.")}
  if (is.null(cluster_ID)) {stop("Must provide a vector from results of a clustering method with the cluster IDs for the species within the data_object.")}
  if (is.null(target_clusters)) {stop("Must provide a vector of target clusters to extract from the cluster results in cluster_ID.")}
  if (usage == "features") {
    if (dim(data_object)[2] != length(cluster_ID)) {stop("The clustering results count must match the data feature of interest.")}
  } else if (usage == "species" | usage == "reduction") {
    if (dim(data_object)[1] != length(cluster_ID)) {stop("The clustering results count must match the data feature of interest.")}
  }
  
  target_index = c()
  for (i in target_clusters) {
    cluster_index = which(cluster_ID == i)
    target_index = c(target_index, cluster_index)
  }
  if (usage == "features") {
    return(colnames(data_object)[target_index])
  } else if (usage == "species" | usage == "reduction") {
    return(rownames(data_object)[target_index])
  }
}
