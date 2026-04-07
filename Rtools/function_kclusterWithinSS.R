## === Analyze the distribution of total withSS for each cluster === ##
# Function to aid in the optimization of kmeans clustering by analyzing the withinSS for esch cluster
# Checks the withSS for cluster for a chosen number of clusters including -1 and +1 of the given k value
# arguments:
  # count_data
  # cluster_count
  # iteration_max
  # kmean_algorithm
  # normalization_comparison

kclusterWithinSS = function (count_data, cluster_count, iteration_max = 10, kmean_algorithm = "Hartigan-Wong", normalize_comparison = FALSE) {
  k_object = kmeans(x = count_data, center = cluster_count-1, iter.max = iteration_max, algorithm = kmean_algorithm)
  clusters = 1:(cluster_count-1)
  if (normalize_comparison == TRUE) {
    withinSS = c(((k_object$withinss)/k_object$tot.withinss)*k_object$tot.withinss/k_object$totss)
  } else {
    withinSS = c((k_object$withinss)/k_object$tot.withinss)
  }
  temp_df1 = cbind(clusters, withinSS)
  #print(clusters)
  #print(withinSS)
  p1 = ggplot(data = temp_df1, aes(x = clusters, withinSS)) + geom_col(fill = "steelblue", color = "black", linewidth = 0.5) + scale_x_continuous(breaks = 1:(cluster_count-1), labels = clusters) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  
  k_object = kmeans(x = count_data, center = cluster_count, iter.max = iteration_max, algorithm = kmean_algorithm)
  clusters = 1:cluster_count
  if (normalize_comparison == TRUE) {
    withinSS = c(((k_object$withinss)/k_object$tot.withinss)*k_object$tot.withinss/k_object$totss)
  } else {
    withinSS = c((k_object$withinss)/k_object$tot.withinss)
  }
  temp_df2 = cbind(clusters, withinSS)
  #print(clusters)
  #print(withinSS)
  p2 = ggplot(data = temp_df2, aes(x = clusters, withinSS)) + geom_col(fill = "steelblue", color = "black", linewidth = 0.5) + scale_x_continuous(breaks = 1:(cluster_count), labels = clusters) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  
  k_object = kmeans(x = count_data, center = cluster_count+1, iter.max = iteration_max, algorithm = kmean_algorithm)
  clusters = 1:(cluster_count+1)
  if (normalize_comparison == TRUE) {
    withinSS = c(((k_object$withinss)/k_object$tot.withinss)*k_object$tot.withinss/k_object$totss)
  } else {
    withinSS = c((k_object$withinss)/k_object$tot.withinss)
  }
  temp_df3 = cbind(clusters, withinSS)
  #print(clusters)
  #print(withinSS)
  p3 = ggplot(data = temp_df3, aes(x = clusters, withinSS)) + geom_col(fill = "steelblue", color = "black", linewidth = 0.5) + scale_x_continuous(breaks = 1:(cluster_count+1), labels = clusters) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  grid.arrange(p1, p2, p3, nrow = 1, ncol = 3, top = "Distribution of withinSS from kmeans Clustering")
} 
