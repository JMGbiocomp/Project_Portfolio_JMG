##
#
#
#
#

optimizeDBSCAN = function (data_object, feature_labels, data_type = "pca", usage = "tune", n_size, hp_check) {
  if (data_type == "pca") {
    hold_data = data_object$x[,1:3]
    colnames(hold_data) = c("PC1", "PC2", "PC3")
  } else if (data_type == "tsne") {
    hold_data = data_object$Y
    colnames(hold_data) = c("tSNE1", "tSNE2")
  } else if (data_type == "umap") {
    hold_data = data_object$layout
    colnames(hold_data) = c("UMAP1", "UMAP2")
  } else if (data_type == "mds") {
    hold_data = data_object$points
    colnames(hold_data) = c("MDS1", "MDS2")
  }
  data_dim = dim(hold_data)[2]
  
  if (usage == "tune") {
    min_points = c(data_dim+1, data_dim*2, data_dim*3, data_dim*10)
    par(mfrow = c(2,2))
    for (p in min_points) {
      kNNdistplot(dist(hold_data), minPts = p)
      title(main = p)
    }
  } else if (usage == "test") {
    if (data_type == "pca") {
      par(mfrow = c(4,3))
    } else {
      par(mfrow = c(2,2))
    }
    min_points = c(data_dim+1, data_dim*2, data_dim*3, data_dim*10)
    for (i in 1:length(n_size)) {
      dbscan_object = dbscan::dbscan(hold_data, eps = n_size[i], minPts = min_points[i])
      cluster_symbols = symbolCode(dbscan_object$cluster)
      feature_colors = colorCode(feature_labels)
      if (data_type == "pca") {
        plot(x = hold_data[,1], y = hold_data[,2], col = feature_colors, xlab = "PC1", ylab = "PC2", main = paste("eps:",n_size[i],"minPts:",min_points[i]), cex = 1, pch = cluster_symbols)
        legend("bottomleft", legend = unique(feature_labels), col = feature_colors, pch = unique(cluster_symbols), bty = "n")
        plot(x = hold_data[,1], y = hold_data[,3], col = feature_colors, xlab = "PC1", ylab = "PC3", main = paste("eps:",n_size[i],"minPts:",min_points[i]), cex = 1, pch = cluster_symbols)
        legend("bottomleft", legend = unique(feature_labels), col = feature_colors, pch = unique(cluster_symbols), bty = "n")
        plot(x = hold_data[,2], y = hold_data[,3], col = feature_colors, xlab = "PC2", ylab = "PC3", main = paste("eps:",n_size[i],"minPts:",min_points[i]), cex = 1, pch = cluster_symbols)
        legend("bottomleft", legend = unique(feature_labels), col = feature_colors, pch = unique(cluster_symbols), bty = "n")
      } else {
        plot(x = hold_data[,1], y = hold_data[,2], col = feature_colors, main = paste("minPts:", min_points[i],"-eps:", n_size[i]), cex = 1, pch = cluster_symbols, )
        legend("bottomleft", legend = unique(feature_labels), col = feature_colors, pch = 16, bty = "n")
      }
    }
    
  } else if (usage == "check") {
    dbscan_object = dbscan::dbscan(hold_data, eps = hp_check[1], minPts = hp_check[2])
    hold_data$cluster = dbscan_object$cluster
    cluster_colors = colorCode(dbscan_object$cluster)
    hold_data$feature = feature_labels
    feature_colors = colorCode(feature_labels)
    
    if (data_type == "pca") {
      c1 = ggplot(data = hold_data, aes(x = PC1, y = PC2, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f1 = ggplot(data = hold_data, aes(x = PC1, y = PC2, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      c2 = ggplot(data = hold_data, aes(x = PC1, y = PC3, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f2 = ggplot(data = hold_data, aes(x = PC1, y = PC3, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      c3 = ggplot(data = hold_data, aes(x = PC2, y = PC3, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f3 = ggplot(data = hold_data, aes(x = PC2, y = PC3, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    } else if (data_type == "tsne") {
      c1 = ggplot(data = hold_data, aes(x = tSNE1, y = tSNE2, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f1 = ggplot(data = hold_data, aes(x = tSNE1, y = tSNE2, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    } else if (data_type == "umap") {
      c1 = ggplot(data = hold_data, aes(x = UMAP1, y = UMAP2, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f1 = ggplot(data = hold_data, aes(x = UMAP1, y = UMAP2, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    } else if (data_type == "mds") {
      c1 = ggplot(data = hold_data, aes(x = MDS1, y = MDS2, color = cluster)) + geom_point(color = cluster_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
      f1 = ggplot(data = hold_data, aes(x = MDS1, y = MDS2, color = feature)) + geom_point(color = feature_colors) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    }
    if (data_type == "pca") {
      gridExtra::grid.arrange(grobs = list(c1,f1,c2,f2,c3,f3), nrow = 3, ncol = 2, main = "DBSCAN Clustering of PCA")
    } else {
      gridExtra::grid.arrange(grobs = list(c1,f1), nrow = 1, ncol = 2, main = paste("DBSCAN Clustering of", data_type))
    }
  }
}