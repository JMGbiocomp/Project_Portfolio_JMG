## === Detection of Global Anomalies by Calculating the Reconstruction Error from PCA Dimensionality Reduction === ##
# Function determines the reconstruction error (root mean squared error) between the reconstructed data from the PCA object and the original data
# Functionality includes a visualization of the RMSE across all features (samples) and retunr of indexes of features that exceed 2 standard deviation above the mean RMSE
# Visualization is intended to aid in the analysis of which features should be considered global anomalies and the return indexes should not be considered outlier without such consideration
# Threshold lines are visualized for 2 SD (blue) and 6 SD (red) above the mean as references for standardized significance criteria and critical criteria, respectively 
# arguments:
  # data_object = matrix-like object of the raw/normalized data with features in columns and species in rows
  # pca_object = data object the result of a prcomp() on the transposed data_object
  # npc = the number of principal components to include in the reconstruction and subsequent analysis; default set to 50 but the pca_object should be analyzed to exclude PC that are likely containing noise variance
# dependencies:
  # stats

analysisRE = function (data_object, pca_object, npc = 50) {
  # reconstruct data and calculate reconstrcution error
  recon_data = t(t(pca_object$x[, 1:npc] %*% t(pca_object$rotation[, 1:npc])) * pca_object$scale + pca_object$center) # reconstructed data from PCA
  errors = (data_object - recon_data)^2 # error matrix (difference^2)
  average_errors = c(rowMeans(errors)) # average errors per data feature 
  
  # build new data structure for analysis and plotting
  RE_data = as.data.frame(cbind(c(1:length(average_errors)), average_errors)) # feature index and RMSE
  colnames(RE_data) = c("Index", "RMSE")
  cutoff_data = RE_data[RE_data$RMSE > mean(RE_data$RMSE)+2*sd(RE_data$RMSE),] # subset data such that only meet the significance threshold (2 SD)
  label_criteria = which(average_errors> mean(average_errors)+2*sd(average_errors)) # get label criteria logic
  
  # visualize RMSE across all feature indexes
  plot(x = RE_data$Index, y = RE_data$RMSE, main = "Detection of Global Anomalies", log = "y", las = 2, xaxt = "n", ylab = "Root Mean Squared Error (RMSE) ", xlab = "Index", ylim = c(mean(average_errors) - sd(average_errors)*2, mean(average_errors) + sd(average_errors)*8), col = "black", cex = 0.5) 
  axis(side = 1, at = label_criteria, labels = colnames(data_object)[label_criteria], cex.axis = 0.5)
  text(x = cutoff_data$Index, y = cutoff_data$RMSE, labels = cutoff_data$Index, pos = 2, cex = 0.5)
  abline(h = median(average_errors), col ="green", lty = "dashed") # median RMSE
  abline(h = mean(average_errors) + sd(average_errors)*2, col = "blue", lty = "dashed") # standard significance threshold
  abline(h = mean(average_errors) + sd(average_errors)*6, col = "red", lty = "dashed") # critical significance threshold (definitive anomalies)
  
  return(cutoff_data$Index) # return all indexes that exceed the 2 SD threshold
}