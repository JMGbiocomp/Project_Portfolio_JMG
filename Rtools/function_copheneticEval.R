### === Evaluation of distance calculations and linkage methods for hierarchical clustering using the correaltion of cophenetic distance and distance matrices === ###
# Function generates a combinatorial matrix of correlation coefficients form the cophenetic distance and distance calculation matrices for every distance calculation and linkage method pair.
# arguments:
  # count_data = count data for hierarchical clustering; functionality extends to non-count data for other variable/distribution types
  # feature_labels = labels for samples for evaluation of clustering of treatment groups and phenotypic characteristics under exploratory analysis
  # dist_calculation = character vector of distance calculation methods for evaluation passed to hclusteringObject function to generate hclust or dendrogram objects
  # linkage_methods = character vector of linkage methods for evaluation passed to hclusteringObject function to generate dendrograms
  # usage = single character vector to distinguish the usage for the function between data QC and exploratory analysis; choose from "Quality Control" or "Exploratory"  

copheneticEval = function (count_data = NA, feature_labels = NA, dist_calculation = c("Euclidean", "Pearson", "Spearman", "Manhattan"), linkage_methods = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), usage = "Quality Control") {
  # errors and flags
  
  if (length(feature_labels) != dim(count_data)[2] & usage != "reduction") {stop("the number of feature labels must equal the number of features (samples)")}
  
  linkage.ct = length(linkage_methods) # number of linkage methods
  dist.ct = length(dist_calculation) # number of distance calculation methods
  
  # generate an output data frame for analysis 
  output_df = data.frame(matrix(0, nrow = dist.ct, ncol = linkage.ct))
  rownames(output_df) = dist_calculation # distance calculation methods as rows
  colnames(output_df) = linkage_methods # linkage methods as columns
  
  # flow control for combinatorial analysis of cophenetic distance of dendrograms
  for (d in dist_calculation) {
    for (l in linkage_methods) {
      h_object = hclustering_object(count_data = count_data, feature_labels = feature_labels, object_type = "hclust", distance_type = d, linkage_method = l, usage = usage, dend_plot = FALSE)
      if (d == "Pearson") {
        if (usage == "reduction") {
          dist_matrix = as.dist(1-stats::cor(count_data))
          dist_matrix = as.matrix(dist_matrix)
        } else {
          dist_matrix = as.dist(1-stats::cor(t(count_data)))          
        }
      } else if (d == "Spearman") {
        if (usage == "reduction") {
          dist_matrix = as.dist(1-stats::cor(count_data, method = "spearman"))
          dist_matrix = as.matrix(dist_matrix)
        } else {
          dist_matrix = as.dist(1-stats::cor(t(count_data), method = "spearman"))          
        }
      } else if (d == "Manhattan") {
        if (usage == "reduction") {
          dist_matrix = dist(count_data, method = "manhattan")
          dist_matrix = as.matrix(dist_matrix)
        } else {
          dist_matrix = dist(t(count_data), method = "manhattan")          
        }
      } else {
        if (usage == "reduction") {
          dist_matrix = dist(count_data, method = "euclidean")
          dist_matrix = as.matrix(dist_matrix)
        } else {
          dist_matrix = dist(t(count_data), method = "euclidean")          
        }
      }
      if (usage == "reduction") {
        coph_matrix = cophenetic(h_object)
        coph_matrix = as.matrix(coph_matrix)
        #print(dim(coph_matrix))
        #print(dim(dist_matrix))
        dist_matrix = as.vector(dist_matrix)
        coph_matrix = as.vector(coph_matrix)
        #print(length(coph_matrix))
        #print(length(dist_matrix))
        coph_coeff = cor(dist_matrix, coph_matrix) # calculation of cophenetic coefficient between cophenetic distacne and distance calculation matrices
        #print(dim(coph_coeff))
        output_df[d,l] = coph_coeff
      } else {
        output_df[d,l] = cor(dist_matrix, cophenetic(h_object)) # calculation of cophenetic coefficient between cophenetic distance and distance calculation matrices
      }
    }
  }
  return(output_df)
}
