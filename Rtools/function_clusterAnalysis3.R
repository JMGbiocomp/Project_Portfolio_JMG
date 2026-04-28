## === Analysis of clustering algorithm applied to data against a given feature === ##
# Function provides data clustering visualization along with a given data feature for comparison
# The applied clustering algorithm captures structure within the data that can validate a data feature of interest 
# Functionality depends on the data object being a result of dimensionality reduction of either PCA, tSNE or UMAP
# Function returns one or more (PCA) ggplots of the reduced data color coded by cluster and provided data feature
# arguments:
  # data_object = dimension-reduced data as either through PCA, tSNE or UMAP
  # feature_labels = vector of values matching row count in data object that corresponds to a data feature of interest
  # cluster_algorithm = indicates which clustering algorithm to use for analysis, either 'hclust' or 'kmeans'
  # cluster_count = number of clusters (centers) to run kmeans clustering
  # reduction = indicates which dimensionality reduction technique was used to create the inputted data object
  # dim_include = number of PC used to run the given clustering algorithm
  # dist_calc = the distance calculation to use for hierarchical clustering 
  # linkage = the linkage method for hierarchical clustering
  # k_algorithm = the algorithm used for kmeans clustering 
# dependencies:
  # stats, ggplot2, gridExtra


clusterAnalysis3 = function (data_object, feature_labels, cluster_algorithm, cluster_count, reduction, dim_include = 10, dist_calc = "euclidean", linkage = "ward.D", k_algorithm = "Hartigan-Wong") {
  # Error and warning flags
  if (length(data_object) == 0) {stop("Need to provide a matrix-like object as data for analysis.")}
  if (length(reduction) == 0) {stop("Must specify whihc dimensionality reduction technique was applied to create the data object: 'pca', 'tsne', or 'umap'")}
  if (length(feature_labels) == 0) {stop("Need to provide a vector of feature labels to analyze.")}
  if (length(cluster_algorithm) == 0) {stop("Must specify which clustering algorithm to use for analysis: 'hclust' or 'kmeans'")}
  if (length(cluster_count) == 0) {stop("Must specify how many clusters to analyze as a integer")}
  message(paste("Distance calculation used for hclust object:", dist_calc))
  message(paste("Hclust linkage method set to", linkage))
  message(paste("kmeans algorithm set to", k_algorithm))
  
  
  if (reduction == "pca") {
    hold_data = data_object$x[,1:dim_include]
  } else if (reduction == "tsne") {
    hold_data = data_object$Y
  } else if (reduction == "umap") {
    hold_data = data_object$layout
  }
  
  if (cluster_algorithm == "hclust") {
    if (dist_calc == "euclidean" | dist_calc == "manhatten") {
      clust_object = stats::hclust(d = dist(hold_data, method = dist_calc), method = linkage)
    } else if (dist_calc == "pearson" | dist_calc == "spearman") {
      clust_object = stats::hclust(d = as.dist(1-stats::cor(t(hold_data), method = dist_calc)), method = linkage)
    }
    clust_result = cutree(clust_object, k = cluster_count)
  } else if (cluster_algorithm == "kmeans") {
    clust_object = stats::kmeans(hold_data, centers = cluster_count, algorithm = k_algorithm)
    clust_result = clust_object$cluster
  }
  
  if (reduction == "pca") {
    plot_data = as.data.frame(data_object$x[,1:3])
    colnames(plot_data) = c("PC1", "PC2", "PC3")
  } else if (reduction == "tsne") {
    plot_data = as.data.frame(data_object$Y)
    colnames(plot_data) = c("tSNE1", "tSNE2")
  } else if (reduction == "umap") {
    plot_data = as.data.frame(data_object$layout)
    colnames(plot_data) = c("UMAP1", "UMAP2")
  }
  
  cluster_colors = colorCode(feature_data = clust_result, color_replace = FALSE)
  print(length(cluster_colors))
  feature_colors = colorCode(feature_data = feature_labels, color_replace = FALSE)
  print(length(feature_colors))
  print(dim(plot_data))
  plot_data$clusters = cluster_colors
  plot_data$features = feature_colors
  print(dim(plot_data))
  
  if (reduction == "pca") {
    p1 = ggplot2::ggplot(data = plot_data, aes(x = PC1, y = PC2, color = clusters)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c1 = ggplot2::ggplot(data = plot_data, aes(x = PC1, y = PC2, color = features)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot2::ggplot(data = plot_data, aes(x = PC1, y = PC3, color = clusters)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c2 = ggplot2::ggplot(data = plot_data, aes(x = PC1, y = PC3, color = features)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p3 = ggplot2::ggplot(data = plot_data, aes(x = PC2, y = PC3, color = clusters)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    c3 = ggplot2::ggplot(data = plot_data, aes(x = PC2, y = PC3, color = features)) + geom_point() + labs(title = "By Feature") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,c1,p2,c2,p3,c3), nrow = 3, ncol = 2, top = "Clustering of PCA Principal Components")
  } else if (reduction == "umap") {
    p1 = ggplot2::ggplot(data = plot_data, aes(x = UMAP1, y = UMAP2, color = clusters)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot2::ggplot(data = plot_data, aes(x = UMAP1, y = UMAP2, color = features)) + geom_point() + labs(title = "By Features") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,p2), nrow = 1, ncol = 2, top = "Clustering of UMAP Reduction")
  } else if (reduction == "tsne") {
    p1 = ggplot2::ggplot(data = plot_data, aes(x = tSNE1, y = tSNE2, color = clusters)) + geom_point() + labs(title = "By Clustering") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    p2 = ggplot2::ggplot(data = plot_data, aes(x = tSNE1, y = tSNE2, color = features)) + geom_point() + labs(title = "By Features") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
    gridExtra::grid.arrange(grobs = list(p1,p2), nrow = 1, ncol = 2, top = "Clustering of tSNE Reduction")
  }
}
