## === Compare Multidimensional scaling (MDS) dimensionality reduction approaches, metric (classic) and an iterative and non-metric algorithm === ##
# Function compares the cmdscale() function from the stats package with the isoMDS() function from the MASS package
# When evaluating samples, euclidean distance is used as the distance/dissimilarity matrix for MDS while the (1-correlation) as distance is used for for evaluating species (genes)
# The "goodness of fit"(GOF) of the only the positive eigenvalues is reported as well as the calculated "stress" (Kruskal's Non-metric Multidimensional Scaling) for the classic and non-metric MDS respectively; GOF above 0.8 and below 0.95 is ideal while the lower the stress (closer to 0) the more accurately the the low dimensionality represents the original data 
# arguments:
  # data_object = a matrix-like data structure transformed and/or normalized with samples/features in columns and species in rows
  # feature_labels = a vector of features corresponding to the features/species of interest within the data; length should match either the samples/features or species of the data structure
  # usage = single character vector indicating if the features (columns) or species (rows) of the data structure are of interest; default set to 'features' ('features' or 'species')
  # data_return = single character vector indicating if and which MDS object should be returned; default set to 'none' ('none', 'classic', or 'non-metric')
# dependencies:
  # stats, MASS

mdsOptimize = function (data_object, feature_labels, usage = "features", data_return = "none") {
  if (usage == "features") {
    classic_mds = cmdscale(d = dist(t(data_object)), list. = TRUE)
    nonmetric_MDS = isoMDS(d = dist(t(data_object)))
    set.seed(100)
    feature_colors = colorCode(feature_data = feature_labels, color_replace = FALSE)
  } else if (usage == "species") {
    classic_mds = cmdscale(d = as.dist(1-cor(t(data_object))), list. = TRUE) # may need to make it abs(correlation) to work correctly
    nonmetric_MDS = isoMDS(d = as.dist(1-cor(t(data_object))))
    feature_colors = colorCode(feature_data = feature_labels, color_replace = TRUE)
  }
  
  par(bg = "white", mfrow = c(1,2), mar = c(8,4,4,5), xpd = TRUE)
  plot(x = classic_mds$points[,1], y = classic_mds$points[,2], col = feature_colors, pch = 16, main = "Metric (Classic) MDS", sub = paste("GOF:",classic_mds$GOF[2]), xlab = "MDS1", ylab = "MDS2")
  legend("bottomleft", legend = unique(feature_labels), col = feature_colors, pch = 16, bty = "n")
  plot(x = nonmetric_MDS$points[,1], y = nonmetric_MDS$points[,2], col = feature_colors, pch = 16, main = "Non-metric MDS", sub = paste("Stress:",nonmetric_MDS$stress), xlab = "MDS1", ylab = "MDS2")
  legend("top", legend = unique(feature_labels), col = feature_colors, pch = 16, bty = "n")
  
  if (data_return == "classic") {
    return(classic_mds)
  } else if (data_return == "non-metric") {
    return(nonmetric_MDS)
  } 
}
