## === Hyper Parameter Tuning of K-means Clustering === ##
# Function provides an in-depth analysis of the number of centers for optimizing k-means clustering
# Functionality extends to include tuning of the number of random starting centers, max iterations, and which centroid-based algorithm is used
# Function can return a summary table to evaluate variance (sum of squares) within and between clusters
# arguments:
  # data_object = matrix-like data structure with feature of interst in rows and ideally the result of a dimensionality reduction technique
  # center_range = numeric vector of the maximum number of centers to evaluate for clustering; default set to 10
  # start_center = numeric vector of the starting random number of centers for the algorithm; default set to 1
  # iterations = numeric vector for the maximum number of iterations to perform for clustering the data; default set to 10
  # k_algorithm = the clustering algorithm to run: "Hartigan-Wong", "Lloyd", "Forgy", or "MacQueen"; default set to "Hartigan-Wong"
  # verbose = logical vector for reporting data analysis tables to the console; default set to FALSE
  # return_data = character vector to determine what data is returned: "summary", "cluster" or "none"; default set to "none"
#

optimizeKmeans = function (data_object, center_range = 10, start_center = 1, iterations = 10, k_algorithm = "Hartigan-Wong", verbose = FALSE, return_data = "none") {
  
  summary_data = data.frame(matrix(0, nrow = center_range, ncol = 6))
  colnames(summary_data) = c("totalSS", "withinSS", "betweenSS/totalSS", "ratioSS", "iter", "centers")
  
  cluster_data = data.frame(matrix(0, nrow = center_range, ncol = center_range))
  colnames(cluster_data) = paste0("cluster",c(1:center_range))
  rownames(cluster_data) = paste("Centers:", c(1:center_range))
  
  for (k in 1:center_range) {
    k_object = kmeans(x = data_object, centers = k, iter.max = iterations, nstart = start_center, algorithm = k_algorithm)
    summary_data[k,"totalSS"] = k_object$totss
    summary_data[k,"withinSS"] = k_object$tot.withinss
    summary_data[k,"betweenSS"] = k_object$betweenss
    summary_data[k,"betweenSS/totalSS"] = k_object$betweenss/k_object$totss
    summary_data[k,"iter"] = k_object$iter
    summary_data[k,"centers"] = length(k_object$size)
    
    for (i in 1:length(k_object$size)) {
      cluster_data[k,i] = k_object$size[i]
    }
    barplot(height = k_object$withinss, width = 1, xlab = "Clusters", ylab = "withinSS per cluster", col = "steelblue", axis.lty = 1)
  }
  
  p1 = ggplot(data = summary_data, aes(x = centers, y = withinSS)) + geom_point() + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  p2 = ggplot(data = summary_data, aes(x = centers, y = betweenSS)) + geom_point() + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  p3 = ggplot(data = summary_data, aes(x = centers, y = betweenSS/totalSS)) + geom_point() + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  gridExtra::grid.arrange(grobs = list(p1,p2,p3), ncol = 3, nrow = 1)
  
  if (verbose) {
    print(cluster_data)
    print(summary_data)
  }
  if (return_data == "summary") {
    return(summary_data)
  } else if (return_data == "cluster") {
    return(cluster_data)
  } else if (return_data == "none") {
    message("No Data Returned for K-means Optimization")
  }
}
