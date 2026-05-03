## === Density-based Detection of Local Outliers === ##
# Function utilizes local outlier factor (LOF), a density-based method, to find probability of data points in the given data space that are dissimilar than those around it 
# LOF is a measure of probability, comparing the density of its position with the density of its neighbors
# LOF scores > 1 are considered potetnial outliers with greater probability of a data point being an outlier the larger the LOF score
#
# arguments:
  # data_object = matrix-like data structure with feature of interest in the rows of the data set; capable of handling both raw/normalized data and data after dimensionality reduction (recommended)
  # k_range = two element numeric vector that provides the range of minimum number of neighbors to evaluate density
  # plot_return = logical value to determine if the LOF scores by feature are visualized for analysis
  # data_return = character value to determine what is returned: ("none", "lof", "index"); "lof" returns the average LOF scores per feature while "index" returns the index of features that exceed the 2 SD threshold; default set to "none" 
# dependencies:
  # stats, dbscan

analysisLOF = function (data_object, k_range = c(10,20), plot_return = FALSE, data_return = "none") {
  lof_average = c(0) # hold the average LOF score per feature
  # flow control to calculate the LOF across the a range of k values
  index = 0
  for (i in k_range[1]:k_range[2]) {
    lof_iter = lof(data_object, minPts = i)
    lof_average = lof_average+lof_iter
    index = index + 1
  }
  lof_average = lof_average/index
  plot_data = as.data.frame(cbind(c(1:length(lof_average)), lof_average))
  colnames(plot_data) = c("Index", "LOF")
  rownames(plot_data) = rownames(data_object)
  cutoff = plot_data[plot_data$LOF > mean(lof_average)+2*sd(lof_average),] # subset data to hold features that exceed the 2 SD threshold
  # flow contorl to visualize LOF scores acorss indexes
  if (plot_return) {
    plot(x = plot_data$Index, y = plot_data$LOF, ylab = "LOF score", xlab = "Index", main = "Detection of Local Outliers", cex = 0.5)
    abline(h = median(lof_average), col = "green", lty = "dashed")
    abline(h = mean(lof_average) + 2*sd(lof_average), col = "blue", lty = "dashed")
    abline(h = mean(lof_average) + 6*sd(lof_average), col = "red", lty = "dashed")
    text(cutoff$Index, cutoff$LOF, labels = cutoff$Index, pos = 2, cex = 0.5)
  }
  # flow control for returned object
  if (data_return == "lof") {
    return(lof_average)
  } else if (data_return == "index") {
    return(cutoff$Index)
  }
}
