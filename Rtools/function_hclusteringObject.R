### === hierarchical clustering of data sets for exploratory analysis === ###
# Function to produce a hierarchical clustering object (hclust) with functionality to output a dendrogram visualization
# Function is intended for initial inspection and lead into distance type/linkage comparisons between two or more dendrograms; functionality for dendrogram plot characteristic adjustments are intentionally excluded
# Arguments:
  # count_data = count data for hierarchical clustering; functionality extends to non-count data for other variable/distribution types
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phentypic characteristics under exploratory analysis
  # object_type = determines the object returned as either a "hclust" or dendrogram ("dend") obejct; default set to "hclust" 
  # distance_type = determines how "distance" is defined for calculations; options include "Manhattan", "Pearson", "Spearman", or "Euclidean"; default is set to "Euclidean"
  # linkage_method = determines how the distance between clusters is calculated to merge them; options include "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid"; must indicate what linkage method to use (default = NA) 
  # dend_plot = logical value to determine if a dendrogram plot is outputted before return of hclust object
  # usage = single character vector to distinguish the usage for the function between data QC and exploratory analysis; choose from "Quality Control" or "Exploratory"
# dependencies
  # stats
  # dendextend

hclustering_object = function (count_data = NA, feature_labels = NA, object_type = "hclust", distance_type = "Euclidean", linkage_method = NA, dend_plot = FALSE, usage = "Quality Control") {
  # errors and flags
  
  
  if (is.na(linkage_method)) {stop("Must choose between the different linkage method choices: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'")}
  if (dend_plot == FALSE) {message("heirarchical clustering not visualized")}
  
  if (usage == "Exploratory") {
    h_data = count_data # will perform hierarchical clustering by species 
  } else if (usage == "Quality Control") {
    h_data = t(count_data) # hclust function requires that features (samples) be rows to evaluate by sample
  } else if (usage == "reduction") {
    h_data = count_data
  }
  
  
  # flow control to handle the distance types and linkage methods
  if (distance_type == "Manhattan") {
    h_object = hclust(d = dist(h_data, method = "manhattan"), method = linkage_method)
  } else if (distance_type == "Pearson") {
    h_object = hclust(d = as.dist(1-stats::cor(h_data)), method = linkage_method)
  } else if (distance_type == "Spearman") {
    h_object = hclust(d = as.dist(1-stats::cor(h_data, method = "spearman")), method = linkage_method)
  } else if (distance_type == "Euclidean") {
    h_object = hclust(d = dist(h_data, method = "euclidean"), method = linkage_method)
  }
  if (object_type == "dend") {
    dend = as.dendrogram(h_object)
    labels(dend) = feature_labels # label dendrogram leaves
    if (dend_plot == TRUE) {
      plot(dend, main = linkage_method) # outputs the plot of the dendrogram
    }
    return(dend)
  } else {
    return(h_object)
  }
}
