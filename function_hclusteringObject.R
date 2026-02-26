### === hierarchical clustering of data sets for exploratory analysis === ###
# Function to produce a hierarchical clustering object (hclust) with functionality to output a dendrogram visualization
# Function is intended for initial inspection and lead into distance type/linkage comparisons between two or more dendrograms; functionality for dendrogram plot characteristic adjustments are intentionally excluded
# Arguments:
  # count_data = count data for hierarchical clustering; functionality extends to non-count data for other variable/distribution types
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phentypic characteristics under exploratory analysis
  # distance_type = determines how "distance" is defined for calculations; options include "Poisson", "Correlation", or "Euclidean"; default is set to "Euclidean"
  # linkage_method = determines how the distance between clusters is calculated to merge them; options include "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid"; must indicate what linkage method to use (default = NA) 
  # dend_plot = logical value to determine if a dendrogram plot is outputted before return of hclust object
  # usage = single character vector to distinguish the usage for the function between data QC and exploratory analysis; choose from "Quality Control" or "Exploratory"

source("function_PackageDependency.R")
package_list = c("stats", "dendextend")
VerifyRequiredPackages(package_list)
library("stats"); library("dendextend")

hclustering_object = function (count_data = NA, feature_labels = NA, distance_type = "Euclidean", linkage_method = NA, dend_plot = FALSE, usage = "Quality Control") {
  # errors and flags
  if (is.na(count_data)) {stop("must include a data set for the 'count_data' arguemnt")}
  if (is.na(feature_labels)) {stop("must provide a vector of labels for features (samples) as the 'feature_labels' argument")}
  if (length(feature_labels) != dim(count_data)[2]) {stop("the number of feature labels must equal the number of features (samples)")}
  if (is.na(linkage_method)) {stop("Must choose between the different linkage method choices: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'")}
  if (dend_plot == FALSE) {message("Plot argument is false and output will not result in a visualiztion of the dendrogram")}
  
  if (usage == "Exploratory") {
    h_data = count_data # will perform hierarchical clustering by species 
  } else if (usage == "Quality Control") {
    h_data = t(count_data) # hclust function requires that features (samples) be rows to evaluate by sample
  }
  
  
  # flow control to handle the distance types and linkage methods
  if (distance_type == "Poisson") {
    dend = as.dendrogram(hclust(PoissonDistance(h_data)$dd, method = linkage_method))
  } else if (distance_type == "Correlation") {
    dend = as.dendrogram(hclust(as.dist(1-cor(t(h_data))), method = linkage_method))
  } else if (distance_type == "Euclidean"){
    dend = as.dendrogram(hclust(dist(h_data), method = linkage_method))
  }
  
  labels(dend) = feature_labels # label dendrogram leaves
  if (dend_plot == TRUE) {
    plot(dend, main = linkage_method) # outputs the plot of the dendrogram
  }
  return(dend)
}