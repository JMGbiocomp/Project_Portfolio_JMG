## === Filtering the Number of PC by Total Variance === ##
# Function determines the number of principal components that captures a defined percentage of the total variance
# Functionality includes a visualization of the cumulative variance from subsequent inclusion of principal components with the cutoff value as a dashed line
# argument:
  # pca_object = a pca object created by the prcomp() function
  # variance_percent = numeric value for the the percentage of total variance to target as a decimal 

filterPC = function (pca_object, variance_percent = 0.8) {
  max_variance = variance_percent*sum(pca_object$sdev^2)
  pc_variance = pca_object$sdev^2
  pc_data = as.data.frame(cbind(c(1:length(pc_variance)), c(0)))
  colnames(pc_data) = c("PC", "Variance")
  
  total_variance = 0
  for (p in 1:length(pc_variance)) {
    total_variance = pca_object$sdev[p]^2 + total_variance
    pc_data[p,"Variance"] = total_variance
  }
  
  cutoff_data = pc_data[pc_data$Variance < max_variance,]
  pc_count = dim(cutoff_data)[1]
  plot_data = pc_data[1:(pc_count+3),]
  plot(x = plot_data[,1], y = plot_data[,2], type = "b", pch = 16, main = "Total Varaince Captured by Inclusion of PC", xlab = "PC", ylab = "Cumulative Variance", col = "black", cex = 0.6)
  abline(h = max_variance, col = "red", lty = "dashed")
  
  return(dim(cutoff_data)[1])
}
