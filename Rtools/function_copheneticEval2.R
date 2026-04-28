## === Evaluation of the cophenetic correlation coefficients between the hierarchical clustering and distance calculations === ##
# Function calculates the cophenetic correlation coefficients for a set of distance calcualtion and linkage methods 
# Ideal for optimizing hierarchical clustering for a given data set
# Functionality includes modifying the data to account for distance calculation algorithm differences
# Works with raw, transformed, normalized, and dimension-reduced data
# arguments:
  # data_object
  # tanspose_data
  # dCalc
  # linkage
# dependencies:
  # stats

copheneticEval2 = function (data_object, transpose_data = "features", data_reduced = FALSE, dCalc = c("euclidean", "pearson", "spearman", "manhattan"), linkage = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), verbose = FALSE) {
  # error and message flags
  if (transpose_data == "features") {
    message("Calculating cophenetic correlation coefficient by features")
  } else {
    message("Calculating cophenetic correlation coefficient by species")
  }
  
  # data set up
  output_data = data.frame(matrix(0, nrow = length(dCalc), ncol = length(linkage)))
  colnames(output_data) = linkage
  rownames(output_data) = dCalc
  
  # flow control for combinatorial analysis pf cophenetic correlation coefficient
  for (d in dCalc) {
    if (verbose) {message(paste("Distance calculation method:", d))}
    for (l in linkage) {
      if (verbose) {message(paste("Linkage method:", l))}
      # perform hierarchical clustering by distance calculation and linkage method
      hclust_object = hclustObject(data_object = data_object, transpose_data = transpose_data, dist_method = d, linkage_method = l, verbose = verbose, reduced = data_reduced)
      # flow control for generating distance matrix needed for correlation assessment 
      if (d == "euclidean" || d == "manhattan") {
        if (transpose_data == "features" & data_reduced != TRUE) {
          data_object = t(data_object)
        }
        d_Matrix = dist(data_object, method = d)
      } else if (d == "pearson" || d == "spearman") {
        if (transpose_data == "species" || data_reduced == TRUE) {
          data_object = t(data_object)
        }
        d_Matrix = as.dist(1-stats::cor(data_object, method = d))
      }
      output_data[d,l] = stats::cor(d_Matrix, cophenetic(hclust_object))
    }
  }
  return(output_data)
}
