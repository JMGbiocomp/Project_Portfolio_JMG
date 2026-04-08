##
#
#
#
#

tsneOptimize = function (data_object, feature_varaible, usage = "explore", dim_output = 2, neighbors = c(5, 10, 15, 30, 50, 100), add_pca = TRUE, theta_value = 0.25) {
  
  if (usage == "explore") {
    par(mfrow = c(2,3))
  } else if (usage == "check") {
    par(mfrow = c(1,1))
  }
  
  groups = unique(feature_varaible) # identify only the unique labels
  set.seed(100)
  colors = sample(x = colors(distinct = TRUE), size = length(groups)) # color code vector to designate feature labels by color
  point_colors = colors[match(feature_varaible, groups)] # generates a color code vector corresponding to each sample's feature
  print(groups)
  
  for (i in neighbors) {
    tsne_object = Rtsne(data_object, dims = dim_output, perplexity = i, pca = add_pca, theta = theta_value)
    plot(x = tsne_object$Y[,1], y = tsne_object$Y[,2], col = point_colors, main = i, cex = 1, pch = 16, xlab = "tSNE1", ylab = "tSNE2")
  }
}
