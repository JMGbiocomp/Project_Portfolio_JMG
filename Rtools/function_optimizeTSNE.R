## === Optimization of tSNE Dimensionality Reduction === ##
# Function provides visualization results from a tSNE object for hyper parameter tuning to balance local and global structure and minimize the Kullback-Leibler (KL) divergence
# Local and global balancing is done through tuning how many nearest neighbors are taken into account when constructing the embedding in the low-dimensional space
#
# Arguments:
  # data_object = matrix-like data structure with feature of interest in rows
  # feature_labels = vector of labels associated with the feature of interest and matches the length of the number of rows in the data_object
  # usage = character value to determine which hyper parameter to analyze, "perplexity", "iterations" or "check"; default set to "perplexity" to analyze balance between local and global structure while "check" allows for evaluating final hyper parameter values or combinatorial assessment if more than one value is provided for each hyper parameter
  # dim_output = integer numeric value to define the number dimensions outputted when creating the tSNE object
  # neighbors = numeric vector of one or more values to analyze the perplexity hyper parameter, range from 5 to 100+; must be a single value to analyze KL divergence; default set to c(5,10,15,30,50,100)
  # iterations = numeric vector of one or more values to analyze the KL divergence after 50 iterations until reaching the max iterations; default set to c(1000,1500,2000,3000,5000)
  # add_pca = logical value to determine if PCA is run prior to tSNE
  # theta_value =  
  # duplicates = logical value determines if duplicates are checked during tSNE
# dependencies:
  # stats, Rtsne

optimizeTSNE = function (data_object, feature_labels, usage = "perplexity", dim_output = 2, neighbors = c(5, 10, 15, 30, 50, 100), iterations = c(1000, 1500, 2000, 3000, 5000), add_pca = TRUE, theta_value = 0.25, duplicates = TRUE) {
  if (usage == "perplexity") {
    message("Max iterations is set to default value (1000) during optimization of perplexity.")
  }
  if (usage == "iterations" & length(neighbors) > 1) {
    stop("To check for stable KL divergence, must optimize perplexity and proivde a single numeric value for 'neighbors' argument.")
  }
  
  for (n in neighbors) {
    if (n*3 == dim(data_object)[1]-1 | n*3 > dim(data_object)[1]-1) {
      stop("A value provided defines a perplexity too large on this particualr data set: '3*perplexity < # of rows - 1")
    }
  }
  
  groups = unique(feature_labels) # identify only the unique labels
  set.seed(100)
  colors = sample(x = colors(distinct = TRUE), size = length(groups), replace = TRUE) # color code vector to designate feature labels by color
  point_colors = colors[match(feature_labels, groups)] # generates a color code vector corresponding to each sample's feature
  # flow control to define the layout and visualizations to tune hyper parameters
  if (usage == "perplexity") {
    # flow control for layout
    if (length(neighbors) == 1) {
      par(mfrow = c(1,2))
    } else {
      par(mfrow = c(abs(length(neighbors)),2))
    }
    # evaluation of perplexity's affect on data stricture in the reduced dimensional space 
    for (i in neighbors) {
      tsne_object = Rtsne(data_object, dims = dim_output, perplexity = i, pca = add_pca, theta = theta_value, check_duplicates = duplicates)
      plot(x = tsne_object$Y[,1], y = tsne_object$Y[,2], col = point_colors, main = i, cex = 1, pch = 16, xlab = "tSNE1", ylab = "tSNE2") # plot of tSNE dimensions
      plot(x = 1:length(tsne_object$itercosts), y = tsne_object$itercosts, col = "black", type = "b", lty = "longdash", xlab = "Number of Iterations (x50)", ylab = "KL Divergence", main = "Embedding Distribution's Dissimilarity") # plot of KL divergence 
    }
  } else if (usage == "iterations") {
    # flow control for layout
    if (length(iterations) == 1) {
      par(mfrow = c(1,2))
    } else {
      par(mfrow = c(abs(length(iterations)),2))
    }
    # evaluation of max iterations' affect on KL divergence
    for (i in iterations) {
      tsne_object = Rtsne(data_object, dims = dim_output, perplexity = neighbors, max_iter = i, pca = add_pca, theta = theta_value, check_duplicates = duplicates)
      plot(x = tsne_object$Y[,1], y = tsne_object$Y[,2], col = point_colors, main = i, cex = 1, pch = 16, xlab = "tSNE1", ylab = "tSNE2") # plot of tSNE dimensions
      plot(x = 1:length(tsne_object$itercosts), y = tsne_object$itercosts, col = "black", type = "b", lty = "longdash", xlab = "Number of Iterations (x50)", ylab = "KL Divergence", main = "Embedding Distribution's Dissimilarity") # plot of KL divergence
    }
  } else if (usage == "check") {
    par(mfrow = c(1,2))
    for (n in neighbors) {
      for (i in iterations) {
        tsne_object = Rtsne(data_object, dims = dim_output, perplexity = n, max_iter = i, pca = add_pca, theta = theta_value, check_duplicates = duplicates)
        plot(x = tsne_object$Y[,1], y = tsne_object$Y[,2], col = point_colors, main = paste("perplexity:", n,"max_iter:", i), cex = 1, pch = 16, xlab = "tSNE1", ylab = "tSNE2") # plot of tSNE dimensions
        plot(x = 1:length(tsne_object$itercosts), y = tsne_object$itercosts, col = "black", type = "b", lty = "longdash", xlab = "Number of Iterations (x50)", ylab = "KL Divergence", main = "Embedding Distribution's Dissimilarity") # plot of KL divergence
      }
    }
  }
}
