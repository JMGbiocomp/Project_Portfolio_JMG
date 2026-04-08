## === Cluster Analysis of Data with Plotting for Visualization === ##
# Function performs a user determined dimension reduction method to a data set before performing a defined clustering algorithm
# Dimensional reduction is limited to PCA, t-SNE, and UMAP techniques
# Function can also take a preprocessed data and immediately apply a clustering algorithm for analysis
# Optimization of dimensinal reduction and clustering parameters is intended with other functions
# arguments:
  #
# dependencies:
  # umap
  # stats
  # Rtsne

clusterPlot = function (data_object, dimension_reduction = "none", cluster_count, cluster_algorithm, alorithm_method, dist_method = "Euclidean", transpose_data = FALSE, dim_output = 2, local_global = 10, dist_min = 0.05) {
  if (dimension_reduction == "pca") {
    if (transpose_data) {
      reduced_data = prcomp(t(data_object), scale = FALSE)                                                                                                                                                                                                                  
    } else {
      reduced_data = prcomp(data_object, scale = FALSE)
    }
    plot_data = as.data.frame(reduced_data$x[,1:dim_output])
    plot_data = cbind(rownames(plot_data), plot_data)
    colnames(plot_data) = c("names","PC1", "PC2", "PC3")
    plot_var = colnames(plot_data)
  } else if (dimension_reduction == "tSNE") {
    if (transpose_data) {
      reduced_data = Rtsne(t(data_object), dims = dim_output, perplexity = local_global, pca = TRUE, theta = 0.25)                                                                                                                                                                                                                
    } else {
      reduced_data = Rtsne(data_object, dims = dim_output, perplexity = local_global, pca = TRUE, theta = 0.25)
    }
    plot_data = as.data.frame(reduced_data$Y)
    plot_data = cbind(rownames(plot_data), plot_data)
    colnames(plot_data) = c("names", "tSNE1", "tSNE2")
    plot_var = colnames(plot_data)
  } else if (dimension_reduction == "UMAP") {
    if (transpose_data) {
      reduced_data = umap(t(data_object), method = "naive", n_neighbors = local_global, n_components = dim_output, min_dist = dist_min, metric = "euclidean")                                                                                                                                                                                                                       
    } else {
      reduced_data = umap(data_object, method = "naive", n_neighbors = local_global, n_components = dim_output, min_dist = dist_min, metric = "euclidean")
    }
    plot_data = as.data.frame(reduced_data$layout)
    plot_data = cbind(rownames(plot_data), plot_data)
    colnames(plot_data) = c("names", "umap1", "umap2")
    plot_var = colnames(plot_data)
  } else if (dimension_reduction == "none") {
    reduced_data = data_object
    plot_data = data_object
    row_names = rownames(plot_data)
    cbind(row_names, plot_data)
    rownames(plot_data) = c("names", row_names)
    plot_var = colnames(plot_data)
  }    
  
  if (cluster_algorithm == "kmeans") {
    cluster_object = kmeans(x = plot_data[,-1], centers = cluster_count, algorithm = alorithm_method)
    cluster_results = cluster_object$cluster
  } else if (cluster_algorithm == "hclust") {
    cluster_object = hclust(d = dist(plot_data[,-1], method = dist_method), method = alorithm_method)
    cluster_results = cutree(cluster_object, k = cluster_count)
  }
  groups = unique(cluster_results) # identify only the unique labels
  colors = sample(x = colors(distinct = TRUE), size = length(groups)) # color code vector to designate feature labels by color
  point_colors = colors[match(cluster_results, groups)] # generates a color code vector corresponding to each sample's feature
  
  if (dimension_reduction == "pca") {
    p1 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = names)) + geom_point(size = 1) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    c1 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = cluster_results)) + geom_point(size = 1) + scale_color_manual(values = point_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    p2 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[4], color = names)) + geom_point(size = 1) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    c2 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = cluster_results)) + geom_point(size = 1) + scale_color_manual(values = point_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    p3 = ggplot(data = plot_data, aes(x = plot_var[3], y = plot_var[4], color = names)) + geom_point(size = 1) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    c3 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = cluster_results)) + geom_point(size = 1) + scale_color_manual(values = point_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    grid.arrange(p1, c1, p2, c2, p3, c3, nrow = 3, ncol = 2, main = "Cluster Analysis of PCs")
  } else {
    p1 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = names)) + geom_point(size = 1) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    c1 = ggplot(data = plot_data, aes(x = plot_var[2], y = plot_var[3], color = cluster_results)) + geom_point(size = 1) + scale_color_manual(values = point_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
    grid.arrange(p1, c1, nrow = 1, ncol = 2, main = "Cluster Analysis of Nonlinear Dimensional Reduction of Data")
  }
  
  if (return == "cluster") {
    return(cluster_object)
  } else if (return == "reduction") {
    return(reduced_data)
  }
}