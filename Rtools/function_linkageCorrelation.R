### === Correlation analysis between linkage methods of the same distance calculation appraoch === ###
# Function provides an analytic reference to aid in the evaluation of linkage methods for the designated distance calculation method for hierarchical clustering. 
# One of several functions to evaluate hierarchical clustering optimization for exploratory analysis
# Argument:
  # count_data = count data for hierarchical clustering; functionality extends to non-count data for other variable/distribution types
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phenotypic characteristics under exploratory analysis
  # dist_calculation = character vector of distance calculation methods for evaluation passed to hclusteringObject function to generate dendrograms; choices for calculations are "Euclidean", "Poisson", or "Correlation"
  # linkage_methods = character vector of linkage methods to analyze correlations between them; default is set to include all linkage methods: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", and "centroid"
  # usage = single character vector to distinguish the usage for the function between data QC and exploratory analysis; choose from "Quality Control" or "Exploratory"  
  # correlation_method =. the correlation coefficient metric to calculate and define the matrix; default is set to cophenetic coeffcient but other options include "baker", "common_nodes", and "FM_index"

library(corrplot)
linkageCorrelation = function (count_data = NA, feature_labels = NA, dist_calculation = NA, linakge_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), usage = "Quality Control", correlation_method = "cophenetic") {
  # errors and flags
  if (is.na(count_data)) {stop("must include a data set for the 'count_data' arguemnt")}
  if (is.na(feature_labels)) {stop("must provide a vector of labels for features (samples) as the 'feature_labels' argument")}
  if (length(feature_labels) != dim(count_data)[2]) {stop("the number of feature labels must equal the number of features (samples)")}
  if (is.na(dist_calculation)) {stop("Must choose a distance calcualtion for hierarchial clustering.")}
  
  dend_list = dendlist() # empty dendlist object
  
  # flow control to manage the series of dendrograms with different linkage methods to fix inside a single dendlist object
  for (l in linkage_methods) {
    dend = hclustering_object(count_data = count_data, feature_labels = feature_labels, distance_type = dist_calculation, linkage_method = l, usage = usage, dend_plot = FALSE) # dendrogram
    temp_list = dendlist(l = dend) # generate a dendlist object with current dendrogram
    dend_list = c(dend_list, temp_list) # add to final dendlist object
  }
  
  correlation_matrix = cor.dendlist(dend_list, method = correlation_method) # produce a correlation matrix between the specified correlation coefficients of each dendrogram wiht the dendlist object
  corrplot(correlation_matrix, method = "pie", type = "lower") # visualization of the correlation matrix
}