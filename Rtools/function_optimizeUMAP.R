## Optimize UMAP Dimensional Reduction of Data
# Function allows the user to tune the local/global ratio of the data within the UMAP algorithm
# Can effectively me used prior to clustering by species or feature
# By default the function explores 6 different values for the n_neighbors argument to probe the optmial value
# arguments:
  # data_object = data to undergo dimensional reduction; data feature under investigation should be the rows of the data
  # usage = defines how the function is used; can be either 'explore' or 'check'; default set to 'explore'
  # neighbors = integer value for the local vs global focus of the dimensional reduction (2-100+); default is c(5, 10, 15, 25, 50, 100)
  # feature_variable = vector of labels or identifiers for each of the species or features under investigation
# dependencies:
  # UMAP

optimizeUMAP = function (data_object, feature_variable, usage = "neighbors", neighbors = c(5, 10, 15, 25, 50, 100), min_distance = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.95), iterations = c(100, 200, 300, 500, 700, 1000), display_legend = FALSE) {
  
  point_colors = codeColor(feature_data = feature_variable)
  
  if (usage == "neighbors") {
    par(mfrow = c(abs(length(neighbors)/2), 2))
    for (i in neighbors) {
      u_object = umap(d = data_object, method = "naive", n_neighbors = i)
      plot(x = u_object$layout[,1], y = u_object$layout[,2], col = point_colors, xlab = "UMAP1", ylab = "UMAP2", cex = 1, pch = 16, main = i)
    }
  } else if (usage == "distance") {
    par(mfrow = c(abs(length(min_distance)/2), 2))  
    for (i in min_distance) {
      u_object = umap(d = data_object, method = "naive", n_neighbors = neighbors, min_dist = i)
      plot(x = u_object$layout[,1], y = u_object$layout[,2], col = point_colors, xlab = "UMAP1", ylab = "UMAP2", cex = 1, pch = 16, main = i)
    }
  } else if (usage == "iterations") {
    par(mfrow = c(abs(length(iterations)/2), 2))
    for (i in iterations) {
      u_object = umap(d = data_object, method = "naive", n_neighbors = neighbors, min_dist = min_distance, n_epochs = i)
      plot(x = u_object$layout[,1], y = u_object$layout[,2], col = point_colors, xlab = "UMAP1", ylab = "UMAP2", cex = 1, pch = 16, main = i)
    }
  } else if (usage == "check") {
    par(mfrow = c(1,1))
    for (n in neighbors) {
      for (d in min_distance) {
        for (i in iterations) {
          u_object = umap(d = data_object, method = "naive", n_neighbors = n, min_dist = d, n_epochs = i)
          plot(x = u_object$layout[,1], y = u_object$layout[,2], col = point_colors, xlab = "UMAP1", ylab = "UMAP2", cex = 1, pch = 16, main = paste("N_neighbors:", neighbors, "min_dist:", min_distance, "n_epoch:", iterations))
          if (display_legend) {legend("bottomleft", legend = unique(feature_variable), col = unique(point_colors), pch = 16, bty = "n")}
        }
      }
    }
  }
}
