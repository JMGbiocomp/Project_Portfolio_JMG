## === Analyze Contribution of Species to Variance in PCA === ##
# Function provides visualizations of to analyze the distribution of the mean pc loadings across all species and the absolute average loading for a defined selection of species
#
# arguments:
  # pca_object = pca object from the pcromp() function
  # species_labels = character vector of species labels for species of interest
  # npcs = numeric vector a length one for the toal number of PCs to include in the analysis
# dependencies:
  # stats

analysisLoadings = function (pca_object, species_labels, npcs) {
  load_data = pca_object$rotation[,1:npcs]
  average_loadings = rowMeans(load_data)
  p1_data = as.data.frame(cbind(c(1:dim(load_data)[1]), average_loadings))
  colnames(p1_data) = c("index", "mean_load")
  rownames(p1_data) = rownames(load_data)
  cutoff_data = p1_data[p1_data$mean_load > mean(p1_data$mean_load)+2*sd(p1_data$mean_load),]
  label_criteria = which(average_loadings > mean(average_loadings)+2*sd(average_loadings) | average_loadings < mean(average_loadings)-2*sd(average_loadings))
  
  par(mfrow = c(1,1))
  plot(x = p1_data$index, y = p1_data$mean_load, main = "Average Directional Weight to Variance", las = 1, xaxt = "n", ylab = "Mean Loading Score", xlab = "Species Index", col = "black", cex = 0.5)
  axis(side = 1, at = label_criteria, labels = rownames(p1_data)[label_criteria], cex.axis = 0.5)
  #text(x = cutoff_data$index, y = cutoff_data$mean_load, labels = cutoff_data$index, pos = 2, cex = 0.5)
  abline(h = mean(average_loadings)+2*sd(average_loadings), lty = "dashed", col = "blue")
  abline(h = mean(average_loadings)-2*sd(average_loadings), lty = "dashed", col = "blue")
  abline(h = mean(average_loadings)+6*sd(average_loadings), lty = "dashed", col = "red")
  abline(h = mean(average_loadings)-6*sd(average_loadings), lty = "dashed", col = "red")
  
  
  average_abs_loadings = rowMeans(abs(load_data))
  p2_data = as.data.frame(cbind(c(1:dim(load_data)[1]), average_abs_loadings))
  colnames(p2_data) = c("index", "abs_data")
  rownames(p1_data) = rownames(load_data)
  p2_data$genes = rownames(p2_data)
  cutoff_data = p2_data[p2_data$abs_data > mean(p2_data$abs_data)+6*sd(p2_data$abs_data),]
  label_criteria = which(average_abs_loadings > mean(average_abs_loadings)+6*sd(average_abs_loadings))
  
  plot(x = 1:length(average_abs_loadings), y = average_abs_loadings, main = "Non-directional Average Weight to Variance", las = 2, xaxt = "n", ylab = "Mean Absolute Loading Score", xlab = "Species of Interest", col = "black", cex = 0.5)
  axis(side = 1, at = cutoff_data$index, labels = cutoff_data$genes, cex.axis = 0.5)
  abline(h = mean(average_abs_loadings)+2*sd(average_abs_loadings), lty = "dashed", col = "blue")
  abline(h = mean(average_abs_loadings)+6*sd(average_abs_loadings), lty = "dashed", col = "red")
  text(cutoff_data$index, cutoff_data$abs_data, labels = cutoff_data$genes, pos = 2, cex = 0.5)
}
   