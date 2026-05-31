### === Correlation analysis between linkage methods of the same distance calculation appraoch === ###
# Function provides an analytic reference to aid in the evaluation of linkage methods for the designated distance calculation method for hierarchical clustering. 
# One of several functions to evaluate hierarchical clustering optimization for exploratory analysis
# Argument:
  # data_object = matrix-like data structure with features in columns and species in rows
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phenotypic characteristics under exploratory analysis
  # usage = character vector to define how to handle the data object; "features" (analyze by features), "species" (analyze by species), "reduced" (dimensionality reduction); default set to "features"
  # dist_calculation = character vector to define the distance metrix: "euclidean", "manhattan", "pearson", "spearman"; default set to c("euclidean", "manhattan", "pearson", "spearman")
  # linkage_methods = character vector of linkage methods to analyze correlations between them; default is set to include all linkage methods: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", and "centroid"
  # correlation_method = the correlation coefficient metric to calculate and define the matrix; default is set to cophenetic coeffcient but other options include "baker", "common_nodes", and "FM_index"
  # display_type = controls waht type of visual representation of the correlation is produced: "circle", "square", "ellipse", "number", "shade", "color", "pie"; default is set to "pie" 
  # verbose = logical vector to define verbosity of function to communicate with the user at the command console
# dependencies
  # dendextend, corrplot

analysisHCLUSTCorrelation = function (data_object = NULL, feature_labels = NULL, usage = "features", dist_calculation = c("euclidean", "manhattan", "pearson", "spearman"), linkage_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), correlation_method = "cophenetic", display_type = "pie", verbose = FALSE) {
  # errors and flags
  if (is.null(data_object)) {stop("Must provide a matrix-like data structure with features in columns and species in rows.")}
  if (is.null(feature_labels)) {stop("Must provide a vector of species labels for determining the minimum threshold.")}
  if (usage == "features" & dim(data_object)[2] != length(feature_labels)) {stop("feature_labels length must match the number of columns in the data_object.")}
  if (dim(data_object)[1] != length(feature_labels) & (usage == "species" | usage == "reduced")) {stop("feature_labels length must match the number of rows in the data_object.")}
  if (verbose) {
    message(paste("Distance calcualtion mehtods under consideration:",dist_calculation))
    message(paste("Linkage methods under consideration:",linkage_methods))
    message(paste("Correlation method used:",correlation_method))
    message(paste("COrrplot display type:",display_type))
  }
  
  dend_list = dendlist() # empty dendlist object\
  label_names = c()
  
  # flow control to manage the series of dendrograms with different linkage methods to fix inside a single dendlist object
  for (d in dist_calculation) {
    for (l in linkage_methods) {
      dend = objectHCLUST(data_object, feature_labels, usage = usage, dist_calculation = d, linkage_method = l, return_object = "dend", dend_plot = FALSE) # dendrogram
      temp_list = dendlist(dend) # generate a dendlist object with current dendrogram
      dend_list = c(dend_list, temp_list) # add to final dendlist object
      label_names = c(label_names, paste(d,l))
    }
  }
  
  dend_list = as.dendlist(dend_list) # fixed issue with dend_list not being a denlist()
  correlation_matrix = cor.dendlist(dend_list, method = correlation_method) # produce a correlation matrix between the specified correlation coefficients of each dendrogram wiht the dendlist object
  colnames(correlation_matrix) = label_names
  rownames(correlation_matrix) = label_names
  corrplot(correlation_matrix, method = display_type, order = "hclust", type = "lower", title = "Correlation Plot by Distance Calculation and Linkage Method") # visualization of the correlation matrix
}
