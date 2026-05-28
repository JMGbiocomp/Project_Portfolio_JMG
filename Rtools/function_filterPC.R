## === Filtering the Number of PC by Total Variance === ##
# Function determines the number of principal components that captures a defined percentage of the total variance
# Functionality includes a visualization of the cumulative variance from subsequent inclusion of principal components with the cutoff value as a dashed line
# argument:
  # pca_object = a pca object created by the prcomp() function
  # variance_percent = numeric value for the the percentage of total variance to target as a decimal 
  # verbose = logical vector to define verbosity of function to communicate with the user at the command console

filterPC = function (pca_object = NULL, variance_percent = 0.8, verbose = FALSE) {
  # errors, warnings, and messages
  if (is.null(pca_object)) {stop("must provide a prcomp object as a pca_object.")}
  if (verbose) {message(paste("Total variance to capture,",(variance_percent*100),"%"))}
  
  max_variance = variance_percent*sum(pca_object$sdev^2) # total variance across all PCs
  pc_variance = pca_object$sdev^2 # calculated variance per PC
  pc_data = as.data.frame(cbind(c(1:length(pc_variance)), c(0))) # build data frame for calculating cumulative variance acorss PCs
  colnames(pc_data) = c("PC", "Variance")
  # flow control to calculate cumulative variance for increasing PC inclusion
  total_variance = 0
  for (p in 1:length(pc_variance)) {
    total_variance = pca_object$sdev[p]^2 + total_variance # total variance acorss current PC range
    pc_data[p,"Variance"] = total_variance
  }
  
  cutoff_data = pc_data[pc_data$Variance < max_variance,] # determines how many PC are needed to capture the user defined variance (variance_percent)
  pc_count = dim(cutoff_data)[1] # total PC count for variance percentage
  plot_data = pc_data[1:(pc_count+3),] # extends the relevent PC for visualization
  #  visualize data with threshold line 
  plot(x = plot_data[,1], y = plot_data[,2], type = "b", pch = 16, main = "Total Varaince Captured by Inclusion of PC", xlab = "PC", ylab = "Cumulative Variance", col = "black", cex = 0.6)
  abline(h = max_variance, col = "red", lty = "dashed")
  
  return(dim(cutoff_data)[1]) # return the number of PC to capture the defined percentage of variance
}
