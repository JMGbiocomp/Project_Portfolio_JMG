## === Filter Species by CPM Normalization === ## 
# Function to filter low count data that is robust enough to apply uniformily to all data sets.
# Counts per million provides a normalization step to all samples and better scale a defined "low count" to a data set despite difference in sample number
# arguments:
  # data_object = matrix-like data structure with features in columns and species in rows
  # feature_labels = character vector for feature labels of equal length to data_object columns
  # verbose = determines verbosity of function with the user at the command console.
# dependencies:
  # stats

filterCPM = function (data_object = NULL, feature_labels = NULL, verbose = TRUE) {
  # errors, warnings, and messages
  if (is.null(data_object)) {stop("Must provide a matrix-like data structure with features in columns and species in rows.")}
  if (is.null(feature_labels)) {stop("Must provide a vector of species labels for determining the minimum threshold.")}
  if (dim(data_object)[2] != length(feature_labels)) {stop("feature_labels length must match the number of features in the data_object.")}
  
  cpm_object = normCPM(data_object = data_object) # CPM normalized data
  cpm_threshold = min(table(feature_labels)) # dynamic threshold fitted to lowest experimental group size
  keep_index = c()
  for (species in 1:dim(cpm_object)[1]) {
    pass_count = 0
    for (sample in 1:dim(cpm_object)[2]) {
      if (cpm_object[species,sample] >= 1) {
        pass_count = pass_count + 1
      }
    }
    if (pass_count >= cpm_threshold) {
      keep_index = c(keep_index, species)
    }
  }
  filtered_species = dim(data_object)[1] - length(keep_index)
  if (vebose) {message(paste("Low count filtering by CPM removed",filtered_species, "species from the data set."))}
  return(data_object[keep_index,])
}
