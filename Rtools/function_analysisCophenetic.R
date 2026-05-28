## === Evaluation of the cophenetic correlation coefficients between the hierarchical clustering and distance calculations === ##
# Function calculates the cophenetic correlation coefficients for a set of distance calculations and linkage methods 
# Ideal for optimizing hierarchical clustering for a given data set
# Functionality includes modifying the data to account for distance calculation algorithm differences
# Works with raw, transformed, normalized, and dimension-reduced data
# arguments:
  # data_object = matrix-like data structure with features in columns and species in rows
  # feature_labels = character or numeric vector of length equal to columns or rows of data_object with feature of interst labels labels
  # usage = character vector to define how to handle the data object; "features" (analyze by features), "species" (analyze by species), "reduced" (dimensionality reduction); default set to "features"
  # dist_calculation = character vector to define the distance calculation method used to create the distance matrix for each hclust object
  # linkage_method = character vector to define the linkage method to use for hclust(): "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"; default set to "average"
  # verbose = logical vector to determine verbosity for communicating with the user at the command console
# dependencies:
  # stats

analysisCophenetic = function (data_object = NULL, feature_labels = NULL, usage = "features", dist_calculation = c("euclidean", "manhattan", "pearson", "spearman"), linkage_method = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), verbose = TRUE) {
  # errors, warnings, and messages
  if (is.null(data_object)) {stop("Must provide a matrix-like data structure with features in columns and species in rows.")}
  if (is.null(feature_labels)) {stop("Must provide a vector of species labels for determining the minimum threshold.")}
  if (usage == "features" & dim(data_object)[2] != length(feature_labels)) {stop("feature_labels length must match the number of columns in the data_object.")}
  if (dim(data_object)[2] != length(feature_labels) & (usage == "species" | usage == "reduced")) {stop("feature_labels length must match the number of rows in the data_object.")}
  
  # output data structure
  summary_data = data.frame(matrix(0, nrow = length(dist_calculation), ncol = length(linkage_method)))
  colnames(summary_data) = linkage_method
  rownames(summary_data) = dist_calculation
  
  if (usage == "features") {
    hold_data = t(data_object)
    if(verbose) {message("Analyzing data by column features")}
  } else if (usage == "species" | usage == "reduced") {
    hold_data = data_object
    if(verbose) {message("Analyzing data by rows")}
  }
  
  for (d in dist_calculation) {
    if(verbose) {message(paste("Analysis for", d, "distance calculation started..."))}
    for (l in linkage_method) {
      hclust_object = objectHCLUST(data_object = data_object, feature_labels = feature_labels, usage = usage, dist_calculation = d, linkage_method = l)
      if (d == "euclidean" | d == "manhattan") {
        dist_object = dist(hold_data, method = d)
      } else if (d == "pearson" | d == "spearman") {
        dist_object = as.dist(1 - stats::cor(t(hold_data), method = d))
      }
      summary_data[d,l] = stats::cor(dist_object, stats::cophenetic(hclust_object))
      if(verbose) {message(paste("Calculation by linkage", l, "complete."))}
    }
    if(verbose) {message(paste("Analysis for", d, "distance calculation complete."))}
  }
  return(summary_data)
}