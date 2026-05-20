## === Extraction of Species by Variance Contribution === ##
# Function extracts major contributors to variance within the data relative the distribution of laodings across all species
# Functionality includes control over if the contribution is directional, non-directional or for a specific subset of principal components
# Each type of evaluation is controled independently and can be adjusted such that the theshold cutoff is unique to each criteria
# arguments:
  # pca_object = 
  # species_labels = 
  # npcs = 
  # d_return = 
  # d_threshold = 
  # nd_return = 
  # nd_threshold = 
  # top_return = 
  # top_pc = 
  # top_threshold = 
# dependencies:
  # stats, MatrixGenerics

filterLoadings = function (pca_object, species_labels, npcs, d_return = TRUE, d_threshold = 2, nd_return = TRUE, nd_threshold = 2, top_return = TRUE, top_pc = c(1:3), top_threshold = 2) {
  
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
    if (average_loading[i] >= average_cutoff_high) {average_index = c(average_index, i)}
    if (average_loading[i] <= average_cutoff_low) {average_index = c(average_index, i)}
    if (absolute_loading[i] >= absolute_cutoff) {absolute_index = c(absolute_index, i)}
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
