## === Extraction of Species by Variance Contribution === ##
# Function extracts major contributors to variance within the data relative the distribution of laodings across all species
# Functionality includes control over if the contribution is directional, non-directional or for a specific subset of principal components
# Each type of evaluation is controled independently and can be adjusted such that the theshold cutoff is unique to each criteria
# arguments:
  # pca_object = data object the result of a prcomp() on the transposed data_object
  # species_labels = character vector for species labels of equal length to data_object rows
  # npcs = numeric vector of a single integer element for the number of PCs to include in the analysis that captures the non-noise variance
  # d_return = logical vector to determine if the species meeting the directional cut off are returned 
  # d_threshold = number of standard deviations to use for setting a threshold to define what is a critical loading for the loading distribution 
  # nd_return = logical vector to determine if the species meeting the non-directional cut off are returned
  # nd_threshold = number of standard deviations to use for setting a threshold to define what is a critical loading for the loading distribution 
  # top_return = logical vector to determine if the species meeting the top PCs only cut off are returned
  # top_pc = numeric vector to determine what PCs shooudl be considered for determining significant species from PC loadings
  # top_threshold = number of standard deviations to use for setting a threshold to define what is a critical loading for the loading distribution 
  # verbose = logical vector to define verbosity of function to communicate with the user at the command console
# dependencies:
  # stats, MatrixGenerics

filterLoadings = function (pca_object = NULL, species_labels = NULL, npcs = NULL, d_return = TRUE, d_threshold = 2, nd_return = TRUE, nd_threshold = 2, top_return = TRUE, top_pc = c(1:3), top_threshold = 2, verbose = FALSE) {
  # errors, warnings, and messages
  if (is.null(pca_object)) {stop("Must provide a prcomp object as the pca_object.")}
  if (is.null(species_labels)) {stop("Must provide a vector of speices labels equal to the row dimensions of the loadings within the pca_object.")}
  if (is.null(npcs)) {stop("Must provide a numeic vector for the number of PCs to include in the loading analysis.")}
  if (dim(pca_object$rotation)[1] != length(species_labels)) {"Species labels count does not match the number of loadings under analysis."}
  if (verbose) {
    message("Species returned are defined by:")
    if (d_return) {message(paste("Average directional loading per species above and below",d_threshold,"standard deviations within the overall distribution."))}
    if (nd_return) {message(paste("Average non-directional loading per species above",nd_threshold,"standard deviations within the overall distribution."))}
    if (top_return) {message(paste("Average directional loading per species above and below",d_threshold,"standard deviations within the overall distribution for PCs",top_pc))}
  }
  
  pca_loading = as.data.frame(pca_object$rotation[,1:npcs]) # extract species loadings
  # directional loading averages and threshold calculations
  average_loading = rowMeans(pca_loading)
  average_cutoff_high = mean(average_loading)+d_threshold*sd(average_loading)
  average_cutoff_low = mean(average_loading)-d_threshold*sd(average_loading)
  # non-directional loading averages and threshold calculations
  absolute_loading = rowMeans(abs(pca_loading))
  absolute_cutoff = mean(absolute_loading)+nd_threshold*sd(absolute_loading)
  # targeted loadings and threshold calculations
  if (length(top_pc) > 1) {
    average_top = rowMeans(pca_loading[,top_pc])
  } else {
    average_top = pca_loading[,top_pc]
  }
  top_cutoff_high = mean(average_top)+top_threshold*sd(average_top)
  top_cutoff_low = mean(average_top)-top_threshold*sd(average_top)
  
  average_index = c() 
  absolute_index = c() 
  top_index = c()
  
  # flow control for identifying species index that meet loading threshold criteria
  for (i in 1:dim(pca_loading)[1]) {
    # directional
    if (average_loading[i] >= average_cutoff_high) {average_index = c(average_index, i)} 
    if (average_loading[i] <= average_cutoff_low) {average_index = c(average_index, i)}
    # non-directional
    if (absolute_loading[i] >= absolute_cutoff) {absolute_index = c(absolute_index, i)}
    # directional for targeted PCs only
    if (average_top[i] >= top_cutoff_high) {top_index = c(top_index, i)}
    if (average_top[i] <= top_cutoff_low) {top_index = c(top_index, i)}
  }
  # flow control to including species index for type of loading criteria
  target_index = c()
  if (d_return) {target_index = c(target_index, average_index)}
  if (nd_return) {target_index = c(target_index, absolute_index)}
  if (top_return) {target_index = c(target_index, top_index)}
  
  target_species = species_labels[unique(target_index)] # only unique species to return
  return(target_species)
}
