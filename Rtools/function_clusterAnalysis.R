## === Analysis of clustering results === ##
# Function performs dimensional reduction on a data set prior to applying a clustering algorithm 
# Graphic is generated with the relevent dimensions plotted by the results of clustering and a provide feature under investigation
# Dimension reduction achived by either PCA, tSNE or UMAP techniques
# Arguments:
  # data_object = a matrix-like data structure for analysis whose rows are the feature intended for investigation
  # feature_labels = vector of values used as identifiers for the feature under investigation
  # cluster_count = number of optimal clusters determined during parameter fine tuning
  # dimension_reduction = character vector of length one for the dimensionality reduction technique ('pca', 'tsne', or 'umap'); default is set to 'pca'
  # cluster_algorithm = character vector of length one for the clustering algorithm ('hclust' or 'kmeans')
  # dist_method = method to use for calculating distances for hierarchical clustering (euclidean, )
  # algorithm_method = character vector of length one for the algorithm method of kmeans clustering ("Hartigan-Wong", "Lloyd", "Forgy","MacQueen") or the linkage method for hclust ("ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC))
  # transpose_data = logical value to transpose the data set or not; feature under investigation must be in the rows of the data structure
  # dim_output = integer for the number of dimensions outputed from the dimensionality reduction 
  # local_global = integer for the perplexity argument for tSNE or n_neighbors argument; parameter adjusts how focused the dimensionality reduction focuses on local or global data structure; default is 10 but should be optimized 
  # dist_min = determines how close points appear in the final layout for umap dimensionality reduction
  # return = character vector for returning an object from the analysis ('reduction', 'cluster', or 'none'); default = "none"

# Dependencies:
  # stats
  # Rtsne
  # umap
  # ggplot2
  # gridExtra

clusterAnalysis = function (data_object, feature_labels, cluster_count, dimension_reduction = "pca", cluster_algorithm, algorithm_method, dist_method = "Euclidean", transpose_data = FALSE, dim_output = 2, local_global = 10, dist_min = 0.05, return = "none") {
  
  if (transpose_data == TRUE) {data_object = t(data_object)}
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
    if (dimension_reduction == "pca") {
      cluster_object = kmeans(x = hold_data, centers = cluster_count, algorithm = algorithm_method)
    } else {
      cluster_object = kmeans(x = reduced_data, centers = cluster_count, algorithm = algorithm_method)
    }
    cluster_results = cluster_object$cluster
  } else if (cluster_algorithm == "hclust") {
    if (dimension_reduction == "pca") {
      cluster_object = hclustering_object(count_data = hold_data, feature_labels = features, object_type = "hclust", distance_type = dist_method, linkage_method = algorithm_method, dend_plot = FALSE, usage = "Exploratory")
    } else {
      cluster_object = hclustering_object(count_data = reduced_data, feature_labels = features, object_type = "hclust", distance_type = dist_method, linkage_method = algorithm_method, dend_plot = FALSE, usage = "Exploratory")
    }
    cluster_results = cutree(cluster_object, k = cluster_count)
  }
  
  groups = unique(cluster_results) # identify only the unique labels
  set.seed(100)
  colors = sample(x = colors(distinct = TRUE), size = length(groups), replace = TRUE) # color code vector to designate feature labels by color
  point_colors = colors[match(cluster_results, groups)] # generates a color code vector corresponding to each sample's feature
  plot_data$cluster_results = point_colors
  set.seed(NULL)
  if (transpose_data == TRUE) {
    groups = unique(features) # identify only the unique labels
    set.seed(1)
    colors = sample(x = colors(distinct = TRUE), size = length(groups), replace = TRUE) # color code vector to designate feature labels by color
    feature_colors = colors[match(features, groups)]
    plot_data$feature_labels = feature_colors
  } else {
    groups = features # all features 
    feature_colors = sample(x = colors(distinct = FALSE), size = length(groups), replace = TRUE) # color code vector to designate feature labels by color
    plot_data$feature_labels = feature_colors
  }
  
  
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
  
  if (return == "reduction") {
    print("Analysis Complete")
    return(reduced_data)
  } else if (return == "cluster") {
    print("Analysis Complete")
    return(cluster_results)
  } else if (return == "none") {
    print("Analysis Complete")
    print("No object was returned from the analysis.")
  }
}
