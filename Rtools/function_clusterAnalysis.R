## === Analysis of clustering results === ##
# Function performs dimensional reduction on a data set prior to applying a clustering algorithm 
# Graphic is generated with the relevent dimensions plotted by the results of clustering and a provide feature under investigation
# Dimension reduction achived by either PCA, tSNE or UMAP techniques
# Arguments:
  # data_object
  # feature_labels
  # cluster_count
  # dimension_reduction
  # cluster_algorithm
  # dist_method
  # transpose_data
  # dim_output
  # local_global
  # dist_min
  # return

# Dependencies:
  # stats
  # Rtsne
  # umap
  # ggplot2
  # gridExtra

clusterAnalysis = function (data_object, feature_labels, cluster_count, dimension_reduction = "pca", cluster_algorithm, alorithm_method, dist_method = "Euclidean", transpose_data = FALSE, dim_output = 2, local_global = 10, dist_min = 0.05, return = "none") {
  
  if (transpose_data) {data_object = t(data_object)}
  features = feature_labels
  
  if (dimension_reduction == "pca") {
    reduced_data = prcomp(x = data_object, scale = FALSE)
    hold_data = as.data.frame(reduced_data$x)
    reduced_data = as.data.frame(reduced_data$x[,1:3])
    col_names = c("names", "PC1", "PC2", "PC3")
  } else if (dimension_reduction == "tsne") {
    reduced_data = Rtsne(data_object, dims = dim_output, perplexity = local_global, pca = TRUE, theta = 0.25, check_duplicates = FALSE)
    reduced_data = as.data.frame(reduced_data$Y)
    col_names = c("names", "tSNE1", "tSNE2")
  } else if (dimension_reduction == "umap") {
    reduced_data = umap(data_object, method = "naive", n_neighbors = local_global, n_components = dim_output, min_dist = dist_min, metric = "euclidean")
    reduced_data = as.data.frame(reduced_data$layout)
    col_names = c("names", "UMAP1", "UMAP2")
  }
  
  plot_data = cbind(features, reduced_data)
  colnames(plot_data) = col_names
  rownames(plot_data) = rownames(data_object)
  
  if (cluster_algorithm == "kmeans") {
    cluster_object = kmeans(x = hold_data, centers = cluster_count, algorithm = alorithm_method)
    cluster_results = cluster_object$cluster
  } else if (cluster_algorithm == "hclust") {
    cluster_object = hclust(d = dist(hold_data, method = dist_method), method = alorithm_method)
    cluster_results = cutree(cluster_object, k = cluster_count)
  }
  
  groups = unique(cluster_results) # identify only the unique labels
  set.seed(100)
  colors = sample(x = colors(distinct = TRUE), size = length(groups)) # color code vector to designate feature labels by color
  point_colors = colors[match(cluster_results, groups)] # generates a color code vector corresponding to each sample's feature
  plot_data$cluster_results = point_colors
  groups = unique(features) # identify only the unique labels
  set.seed(1)
  colors = sample(x = colors(distinct = TRUE), size = length(groups)) # color code vector to designate feature labels by color
  feature_colors = colors[match(features, groups)]
  plot_data$feature_labels = feature_colors
  
  if (dimension_reduction == "pca") {
    p1 = ggplot(data = plot_data, aes(x = PC1, y = PC2, color = point_colors)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c1 = ggplot(data = plot_data, aes(x = PC1, y = PC2, color = feature_colors)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot(data = plot_data, aes(x = PC1, y = PC3, color = point_colors)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c2 = ggplot(data = plot_data, aes(x = PC1, y = PC3, color = feature_colors)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p3 = ggplot(data = plot_data, aes(x = PC2, y = PC3, color = point_colors)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c3 = ggplot(data = plot_data, aes(x = PC2, y = PC3, color = feature_colors)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,c1,p2,c2,p3,c3), nrow = 3, ncol = 2, top = "Clustering of PCA Principal Components")
  } else if (dimension_reduction == "umap") {
    p1 = ggplot(data = plot_data, aes(x = UMAP1, y = UMAP2, color = point_colors)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot(data = plot_data, aes(x = UMAP1, y = UMAP2, color = feature_labels)) + geom_point() + labs(title = "By Features") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,p2), nrow = 1, ncol = 2, top = "Clustering of UMAP Reduction")
  } else if (dimension_reduction == "tsne") {
    p1 = ggplot(data = plot_data, aes(x = tSNE1, y = tSNE2, color = point_colors)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot(data = plot_data, aes(x = tSNE1, y = tSNE2, color = feature_labels)) + geom_point() + labs(title = "By Features") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,p2), nrow = 1, ncol = 2, top = "Clustering of tSNE Reduction")
  }
  
}
