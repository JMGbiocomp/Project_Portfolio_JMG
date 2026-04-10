



function (data_object, feature_labels, transpose_data = FALSE, cluster_algorithim, cluster_count, reduction = "pca", dim_include = 10, dist_calc = "euclidean", linkage = "ward.D") {
  if (transpose_data) {
    data_object = t(data_object)
  }
  if (reduction = "pca") {
    hold_data = data_object$x[,1:dim_include]
  } else if (reduction = "tsne") {
    hold_data = data_object$Y
  } esle if (reduction = "umap") {
    hold_data = data_object$layout
  }
  
  if (cluster_algorithim == "hclust") {
    clust_object = hclustObject(data_object = hold_data, feature_labels = feature_labels, transpose_data = FALSE, dist_method = dist_calc, linkage_method = linkage, dend_plot = FALSE, object_return = "hclust")
    clust_result = cutree(clust_object, k = cluster_count)
  } else if (cluster_algorithim == "kmeans") {
    clust_object = kmeans()
    clust_result = clust_object$cluster
  }
  
  if (reduction = "pca") {
    plot_data = data_object$x[,1:3]
  } else if (reduction = "tsne") {
    plot_data = data_object$Y
  } else if (reduction = "umap") {
    plot_data = data_object$layout
  }
  
  
  
}