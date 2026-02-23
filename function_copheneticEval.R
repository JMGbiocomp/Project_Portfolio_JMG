### === Evaluation of distance calculations and linkage methods for hierarchical clustering using cophenetic distance  === ###
# Function generates a combinatorial matrix of cophenetic distance values for every distance calculation and linkage method pair.
# arguments:
  # count_data = count data for hierarchical clustering; functionality extends to non-count data for other variable/distribution types
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phenotypic characteristics under exploratory analysis
  # dist_calculation = character vector of distance calucaltion methods for evaluation passed to hclusteringObject function to generate dendrograms
  # linkage_methods = character vector of linkage methods for evaluation passed to hclusteringObject function to generate dendrograms

source("function_hclustering_object.R")

copheneticEval = function (count_data = NA, feature_labels = NA, dist_calculation = c("Euclidean", "Poisson", "Correlation"), linakge_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")) {
  # errors and flags
  if (is.na(count_data)) {stop("must include a data set for the 'count_data' arguemnt")}
  if (is.na(feature_labels)) {stop("must provide a vector of labels for features (samples) as the 'feature_labels' argument")}
  if (length(feature_labels) != dim(count_data)[2]) {stop("the number of feature labels must equal the number of features (samples)")}
  
  linkage.ct = length(linkage_methods) # number of linkage methods
  dist.ct = length(dist_calculation) # number of distance calculation methods
  
  # generate an output data frame for analysis 
  output_df = data.frame(matrix(0, nrow = dist.ct, ncol = linkage.ct))
  rownames(output_df) = dist_calculation # distance calculation methods as rows
  colnames(output_df) = linkage_methods # linkage methods as columns
  
  # flow control for combionatorial analysis of cophenetic distance of dendrograms
  for (d in dist_calculation) {
    for (l in linakge_methods) {
      dend = hclustering_object(count_data = count_data, feature_labels = feature_labels, distance_type = d, linkage_method = l, dend_plot = FALSE)
      output_df[d,l] = cophenetic(dend) # calculation cophenetic distance
    }
  }
  return(output_df)
}