##
#
#
#
#

analysisSpecies = function (data_object, cluster_assignment, extract_cluster) {
  
  hold_data = as.data.frame(data_object)
  hold_data$cluster = cluster_assignment
  cluster_colors = codeColor(cluster_assignment)
  hold_data$colors = cluster_colors
  
  p1 = ggplot(data = hold_data, aes(x = colnames(hold_data)[1], y = colnames(hold_data)[2], color = factor(cluster))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
  p1
  table(cluster_assignment)
  
  cluster_choices = unique(cluster_assignment)
  target_data = hold_data[hold_data$cluster == cluster_choices[extract_cluster],]
  return(rownames(target_data))
}

analysisCluster = function (data_object, data_dimensions = 2, feature_labels, usage = "features", reduction = "pca", clustering = "hclust", h_distance = "euclidean", h_linkage = "average", center_count, k_algorithm = "Hartigan-Wong", iterations = 10, db_min = dim(data_object)[2]+1, db_radius = 1) {
  
  if (reduction == "pca") {
    hold_data = as.data.frame(data_object$x[,1:data_dimensions])
  } else if (reduction == "tsne") {
    hold_data = as.data.frame(data_object$Y[,1:data_dimensions])
    colnames(hold_data) = c("Dim1", "Dim2")
  } else if (reduction == "umap") {
    hold_data = as.data.frame(data_object$layout[,1:data_dimensions])
    colnames(hold_data) = c("Dim1", "Dim2")
  } else if (reduction == "mds") {
    hold_data = as.data.frame(data_object$points[,1:data_dimensions])
    colnames(hold_data) =c("Dim1", "Dim2")
  }
  
  if (clustering == "hclust") {
    cluster_object = objectHCLUST((data_object = hold_data, feature_labels = feature_labels, usage = usage, dist_calculation = h_distance, linkage_method = h_linkage, return_object = "dend", dend_plot = FALSE))
    cluster_assignment = cutree(cluster_object, k = center_count)
  } else if (clustering == "kmeans") {
    cluster_object = kmeans(x = hold_data, centers = center_count, iter.max - iterations, algorithm = k_algorithm)
    cluster_assignment = cluster_object$cluster
  } else if (clustering == "dbscan") {
    cluster_object = dbscan(x = hold_data, eps = db_radius, minPts = db_min)
    cluster_assignment = cluster_object$cluster
  }
  
  if (reduction == "pca") {
    hold_data = hold_data[,1:3]
    colnames(hold_data) = c("PC1", "PC2", "PC3")
  } 
  hold_data$clusters = cluster_assignment
  hold_data$labels = feature_labels
  
  if (usage == "features") {
    if (reduction == "pca") {
      f1 = ggplot(data = hold_data, aes(x = PC1, y = PC2, color = factor(labels))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      f2 = ggplot(data = hold_data, aes(x = PC1, y = PC3, color = factor(labels))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      f3 = ggplot(data = hold_data, aes(x = PC2, y = PC3, color = factor(labels))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      c1 = ggplot(data = hold_data, aes(x = PC1, y = PC2, color = factor(clusters))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      c2 = ggplot(data = hold_data, aes(x = PC1, y = PC3, color = factor(clusters))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      c3 = ggplot(data = hold_data, aes(x = PC2, y = PC3, color = factor(clusters))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      gridExtra::grid.arrange(grobs = list(f1,f2,f3,c1,c2,c3), nrow = 2, ncol = 3)
    } else {
      f1 = ggplot(data = hold_data, aes(x = Dim1, y = Dim2, color = factor(labels))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      c1 = ggplot(data = hold_data, aes(x = Dim1, y = Dim2, color = factor(clusters))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
      gridExtra::grid.arrange(grobs = list(f1,c1), nrow = 1, ncol = 2)
    }
  } else if (usage == "species") {
    f1 = ggplot(data = hold_data, aes(x = Dim1, y = Dim2, color = factor(labels))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
    c1 = ggplot(data = hold_data, aes(x = Dim1, y = Dim2, color = factor(clusters))) + geom_point() + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_blank(), legend.position = "bottom")
    gridExtra::grid.arrange(grobs = list(f1,c1), nrow = 1, ncol = 2)
  }
  print("Cluster Assignments:")
  table(hold_data$clusters)
  if (usage == "features") {
    print("Feature Assignments:")
    table(hold_data$labels)
    }
  return(hold_data)
}