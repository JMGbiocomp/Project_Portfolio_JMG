## === Identify Significant Species Contributing to Variance === ##
# Function analyzes the average PC loadings across all species to identify the most prominent contributors to the variance across all axes
# Functionality includes visualizations and the use of two different metrics for assessment: directional average and non-directional average of PC loadings between all PCs
# User can specify the standard deviation boundary for identifying significant contributors to further refine how critical their contribution is to the variance
# arguments:
  # pca_object = PCA object from the prcomp() function 
  # species_labels = character vector of the species names for the species found within the PC loading data of equal length 
  # npcs = numeric vector for the number of PCs to include from the original PCA object
  # std_cutoff = numeric vector for the number of standard deviations to use as a cutoff of significance; default set to 2
  # plot_labels = logical vector to determine if visualizations are generated with point labels; default set to FALSE
  # return_data = character vector to control what vmetric is used for identifying significant contributors: "average" (directional), "absolute" (non-directional) or "joint"(directional and non-directional); default set to "joint"
# dependencies:
  # stats

filterPCL = function (pca_object, species_labels, npcs, std_cutoff = 2, plot_labels = FALSE, return_data = "joint") {
  # errors, warnings, and messages
  if(is.null(pca_object)) {stop("Must provide a prcomp object as a pca_object.")}
  if(is.null(species_labels)) {stop("Must provide species labels matching the rows of the PC loadings wihtin the pca_object.")}
  if(is.null(npcs)) {stop("Must provide the total number of PCs to frame the variance contribution of the species.")}
  warning(paste("Critical loading value determined by",std_cutoff,"standard deviations within the distribution."))
  
  pc_loadings = as.data.frame(pca_object$rotation[,1:npcs]) # extracted PC laoding data for the number of defined PCs
  species_names = species_labels # names of species
  species_index = 1:length(species_names) # species index in original data set
  average_loading = c(rowMeans(pc_loadings)) # directional average of PC loadings per species
  abs_loading = c(rowMeans(abs(pc_loadings))) # non-directional average of PC loadings per species
  plot_data = data.frame(matrix(0, ncol = 4, nrow = length(species_index))) # plot data for summary statisics 
  colnames(plot_data) = c("species", "index", "average", "absolute") 
  # define summary data structure 
  plot_data$species = species_names
  plot_data$index = species_index
  plot_data$average = average_loading
  plot_data$absolute = abs_loading
  
  # subset summary data by standard deviation criteria 
  average_criteria = plot_data[(plot_data$average > mean(plot_data$average)+std_cutoff*sd(plot_data$average) | plot_data$average < mean(plot_data$average)-std_cutoff*sd(plot_data$average)),]
  absolute_criteria = plot_data[(plot_data$absolute > mean(plot_data$absolute)+std_cutoff*sd(plot_data$absolute)),]
  
  # data visualization
  par(mfrow = c(1,2))
  plot(x = plot_data$index, y = plot_data$average, main = "Directional Average Contribution to Variance", cex = 0.5, xlab = "Species Index", ylab = "Average PC Loading") # directional average plot
  abline(h = median(average_loading), col = "green", lty = "dashed") # median
  abline(h = mean(average_loading) + std_cutoff*sd(average_loading), col = "blue", lty = "dashed") # significance cutoff in blue
  abline(h = mean(average_loading) - std_cutoff*sd(average_loading), col = "blue", lty = "dashed") # significance cutoff in blue
  abline(h = mean(average_loading) + 6*sd(average_loading), col = "red", lty = "dashed") # predefined critical cutoff
  abline(h = mean(average_loading) - 6*sd(average_loading), col = "red", lty = "dashed") # predefined critical cutoff
  if (plot_labels) {
    text(average_criteria$index, average_criteria$average, labels = as.character(average_criteria$index), pos = 2, cex = 0.5) # labels
  }
  
  plot(x = plot_data$index, y = plot_data$absolute, main = "Non-directional Average Contribution to Variance", cex = 0.5, xlab = "Species Index", ylab = "Average Absolute PC Loading") # non-directional average plot
  abline(h = median(abs_loading), col = "green", lty = "dashed") # median
  abline(h = mean(abs_loading) + std_cutoff*sd(abs_loading), col = "blue", lty = "dashed") # significance cutoff in blue
  abline(h = mean(abs_loading) + 6*sd(abs_loading), col = "red", lty = "dashed") # predefined critical cutoff
  if (plot_labels) {
    text(absolute_criteria$index, absolute_criteria$absolute, labels = as.character(absolute_criteria$index), pos = 2, cex = 0.5) # labels 
  }
  
  # flow control for returning identified species by metric
  if (return_data == "average") {
    target_species = c(average_criteria$species) 
  } else if (return_data == "absolute") {
    target_species = c(absolute_criteria$species)
  } else if (return_data == "joint") {
    target_species = unique(c(average_criteria$species, absolute_criteria$species))
  }
  return(target_species)
}