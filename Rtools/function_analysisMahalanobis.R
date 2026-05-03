## === Local Outlier Detection by Mahalanobis Distance for Defined Groupings/Clusters === ##
# Function utilizes the mahalanobis distance from a calculated center based on defined data groups or clusters to detect local outliers and determines significance by a chi-squared distribution
# Groups can be defined by data feature or clustering results
# Input data must be two dimensional either by PCA, tSNE, or UMAP dimensionality reduction techniques

# arguments:
  # data_object = matrix-like data structure with feature of interest in rows and dimensional coordinates in columns; Output from dimensionality reduction techniques: PCA ($x), tSNE ($Y), and UMAP ($layout) accepted.
  # feature_labels = groupings or clustering of the feature of interest to frame the local outlier algorithm against to find data points dissimilar to others of its saem group/cluster
# dependencies:
  # stats


analysisMahalabonis = function (data_object, feature_labels) {
  # configure data with key elements for visualization and returning feature indexes 
  hold_data = as.data.frame(data_object) # build data frame
  hold_data$features = feature_labels # add feature labels
  hold_data$sample = rownames(hold_data) # add feature names
  hold_data$Index = 1:dim(hold_data)[1] # add indexes
  hold_data$distance = c(0) # add place holder for distance metrics per feature
  groups = unique(feature_labels) # determine unique feature labels
  
  # flow control to establish visualization gridding
  if (length(groups) %% 2 == 0) {
    if (length(groups) == 2) {
      par(mfrow = c(1,length(groups)))
    } else if (length(groups) < 8) {
      par(mfrow = c(2,length(groups)/2))
    } else if (length(groups) == 8) {
      par(mfrow = c(4,length(groups)/4))
    } else if (length(groups) == 10) {
      par(mfrow = c(5,length(groups)/5))
    }
  } else if (length(groups) > 2) {
    if (length(groups) == 3) {
      par(mfrow = c(1,3))
    } else {
      par(mfrow = c((length(groups)-1)/2, abs(length(groups)/2)))
    }
  } else {
    par(mfrow = c(1,1))
  }
  # flow control to analyze mahalanobis distance by feature for the given data object
  index_outliers = c()
  for (i in 1:length(groups)) {
    feature_data = hold_data[hold_data$feature == groups[i],] # separate data by current feature
    feature_data$distance = mahalanobis(x = feature_data[,1:2], colMeans(feature_data[,1:2]), cov = cov(feature_data[,1:2])) # calcualte mahalanobis distance
    threshold = qchisq(p = 0.999, df = ncol(feature_data)) # determine outlier threshold by the chi-squared distribution
    target_data = subset(feature_data, feature_data$distance > threshold) # define outliers by threshold
    index_outliers = c(index_outliers, target_data$Index) # add indexes of found outlier to return variable
    label_criteria = which(feature_data$distance > threshold) # define which feature points will be labeled in visualization
    # plot feature mahalanobis distances against whole data object indexes
    plot(x = feature_data$Index, y = feature_data$distance, main = paste("Detection of Local Outliers (LOD):",groups[i] ), ylim = c(0,threshold+10), ylab = "Mahalanobis Distance", xlab = "Index", col = "black", cex = 0.8) 
    # flow control to handle instances where no distance value exceeds the determined threshold
    if( length(label_criteria)<1) {
      
    } else {
      text(x = target_data$Index, y = target_data$distance, labels = label_criteria, pos = 3, cex = 0.8)
    } 
    abline(h = median(feature_data$distance), col ="green", lty = "dashed") # reference line for median value of distances
    abline(h = threshold, col = "red", lty = "dashed") # visualize the threshold value across indexes
    
    # currently an artifact of a previous return variable and kept for futre iterations
    index = 1
    for (r in 1:dim(hold_data)[1]) {
      if (hold_data[r,"features"] == groups[i]) {
        hold_data[r, "distance"] = feature_data[index,"distance"]
        index = index + 1
      } 
    }
  }
  par(mfrow = c(1,1)) # redefine the default visualization grid
  return(index_outliers) # returns outlier indexes that exceeded the threshold
}

