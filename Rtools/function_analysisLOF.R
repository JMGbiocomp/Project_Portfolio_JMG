## === Density-based Detection of Local Outliers === ##
# Function utilizes local outlier factor (LOF), a density-based method, to find probability of data points in the given data space that are dissimilar than those around it 
# LOF is a measure of probability, comparing the density of its position with the density of its neighbors
# LOF scores > 1 are considered potetnial outliers with greater probability of a data point being an outlier the larger the LOF score
#
# arguments:
  # data_object = matrix-like data structure with feature of interest in the rows of the data set; capable of handling both raw/normalized data and data after dimensionality reduction (recommended)
  # k_range = two element numeric vector that provides the range of minimum number of neighbors to evaluate density
# dependencies:
  # dbscan

analysisLOF = function (data_object, k_range = c(10,20), plot_return = FALSE) {
  lof_average = c(0)
  index = 0
  for (i in k_range[1]:k_range[2]) {
    lof_iter = lof(data_object, minPts = i)
    lof_average = lof_average+lof_iter
    index = index + 1
  }
  lof_average = lof_average/index
  if (plot_return) {
    plot_data = as.data.frame(cbind(c(1:length(lof_average)), lof_average))
    colnames(plot_data) = c("Index", "LOF")
    rownames(plot_data) = rownames(data_object)
    plot(x = plot_data$Index, y = plot_data$LOF, ylab = "LOF score", xlab = "Index", main = "Detection of Local Outliers", cex = 0.5)
    abline(h = mean(lof_average), col = "green", lty = "dashed")
    abline(h = mean(lof_average) + 2*sd(lof_average), col = "blue", lty = "dashed")
    abline(h = mean(lof_average) + 6*sd(lof_average), col = "red", lty = "dashed")
    cutoff = plot_data[plot_data$LOF > mean(lof_average)+2*sd(lof_average),]
    text(cutoff$Index, cutoff$LOF, labels = cutoff$Index, pos = 2, cex = 0.5)
  }
  return(lof_average)
}
