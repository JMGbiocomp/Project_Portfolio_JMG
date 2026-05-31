## === Performs Hierarchical Clustering on a Data Object === ##
# Function has a buitl in algorithm to execute hclust() on a data object by feature or species and implements the appropraite distacne calculation while using the defined linkage method
# Functionality includes visualization of a dendrogram and can return the hclust object or dendrogram object
# arguments:
  # data_object = matrix-like data structure with features in columns and species in rows
  # feature_labels = character or numeric vector of length equal to columns or rows of data_object with feature of interst labels 
  # usage = character vector to define how to handle the data object; "features" (analyze by features), "species" (analyze by species), "reduced" (dimensionality reduction); default set to "features"
  # dist_calculation = character vector to define the distance metrix: "euclidean", "manhattan", "pearson", "spearman"; default set to "euclidean"
  # linkage_method = character vector to define the linakge method to use for hclust(): "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"; default set to "average"
  # return_object = character vector to determine what object is returned: "hclust" (hclust object) of "dend" (dendrogram); default set to "hclust"
  # dend_plot = logical value to determine if the dendrogram is visualized
# dependencies
  # stats

objectHCLUST = function (data_object = NULL, feature_labels = NULL, usage = "features", dist_calculation = "euclidean", linkage_method = "average", return_object = "hclust", dend_plot = FALSE) {
  # errors, warnings, and messages
  if (is.null(data_object)) {stop("must provide a matrix-like data structure with features in columns and species in rows.")}
  if (is.null(feature_labels)) {stop("must provide a vector of length equal to the feature of interest within data_object.")}
  
  # flow control to determine data structure needed for creating hclust object
  if (usage == "features") {
    if (dim(data_object)[2] != length(feature_labels)) {stop("feature labels must be the same length as the number of features in data_object.")}
    hold_data = t(data_object) # features in rows
  } else if (usage == "species" | usage == "reduced") {
    if (dim(data_object)[1] != length(feature_labels)) {stop("feature labels must be the same length as the number of species in data_object.")}
    hold_data = data_object # species or target of interest from dimesnionality reduction in rows
  }
  
  # flow control to build the correct distance matrix and generate the hclust object with the correct linkage method
  if (dist_calculation == "euclidean" | dist_calculation == "manhattan") {
    h_object = hclust(d = dist(hold_data, method = dist_calculation), method = linkage_method)
  } else if (dist_calculation == "pearson" | dist_calculation == "spearman") {
    hold_data = t(hold_data) # transpose for cor() function (produces correaltion by columns of data object)
    h_object = hclust(d = as.dist(1-stats::cor(hold_data, method = dist_calculation)), method = linkage_method)
  }
  
  if (return_object == "hclust") {
    return(h_object)
  } else if (return_object == "dend") {
    dend = as.dendrogram(h_object)
    labels(dend) = feature_labels # label dendrogram leaves
    if (dend_plot) {
      plot(dend, main = paste("Dendrogram of Data-Distance:", dist_calculation,"-Linkage:", linkage_method))
    }
    return(dend)
  }
}
