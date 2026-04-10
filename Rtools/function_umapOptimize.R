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

umapOptimize = function (data_object, feature_varaible, usage = "explore", neighbors = c(5, 10, 15, 25, 50, 100)) {
  
  if (usage == "explore") {
    par(mfrow = c(2,3))
  } else if (usage == "check") {
    par(mfrow = c(1,1))
  }
  
  groups = unique(feature_varaible) # identify only the unique labels
  set.seed(100)
  colors = sample(x = colors(distinct = TRUE), size = length(groups), replace = TRUE) # color code vector to designate feature labels by color
  point_colors = colors[match(feature_varaible, groups)] # generates a color code vector corresponding to each sample's feature
  
  for (i in neighbors) {
    u_object = umap(d = data_object, method = "naive", n_neighbors = i)
    plot(x = u_object$layout[,1], y = u_object$layout[,2], col = point_colors, xlab = "UMAP1", ylab = "UMAP2", cex = 1, pch = 16, main = i)
  }
}
