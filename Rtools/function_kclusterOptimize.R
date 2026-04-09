## === Analysis of total withinSS and the betweenSS to totalSS ratio === ##
# Function to aid in the optimization of kmeans clustering by analyzing how cluster count changes total withinSS and the total betweenSS to totalSS ratio
# Provides a graphic of these metircs across a range of cluster counts  
# arguments:
  # count_data
  # clustering_range
  # iteration_max
  # kmean_algorithm


kclusterOptimize = function (count_data, cluster_range, iteration_max = 10, kmean_algorithm = "Hartigan-Wong", return = FALSE) {
  temp_df = data.frame(matrix(0, nrow = cluster_range, ncol= 5))
  colnames(temp_df) = c("k","totalSS", "total.withinSS", "total.betweenSS", "ratioSS")
  
  for (c in 1:cluster_range) {
    k_object = kmeans(x = count_data, center = c, iter.max = iteration_max, algorithm = kmean_algorithm)
    temp_df[c,"k"] = c
    temp_df[c,"totalSS"] = k_object$totss
    temp_df[c,"total.withinSS"] = k_object$tot.withinss
    temp_df[c,"total.betweenSS"] = k_object$betweenss
    temp_df[c,"ratioSS"] = k_object$betweenss/k_object$totss
  }
  p1 = ggplot(data = temp_df, aes(x = k, y = ratioSS)) + geom_col(fill = "steelblue", color = "black", linewidth = 0.5) + scale_x_continuous(breaks = 1:cluster_range, labels = 1:cluster_range) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  p2 = ggplot(data = temp_df, aes(x = k, y = total.withinSS)) + geom_col(fill = "steelblue", color = "black", linewidth = 0.5) + scale_x_continuous(breaks = 1:cluster_range, labels = 1:cluster_range) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  gridExtra::grid.arrange(p1, p2, nrow = 1, ncol = 2, top = "Analysis of Sum of Squares for kmeans Clustering")
  if (return == TRUE) {
    return(temp_df)
  }
}
