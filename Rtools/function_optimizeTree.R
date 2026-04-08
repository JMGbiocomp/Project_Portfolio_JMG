## === Optimize the hierachical clustering tree groups === ##
# Function provides a report of how leaves are grouped by cluster and their matching to a feature varaible
# intended to optimize the cutting of a dendrogram, which aids in the analysis of the hierachical clsutering apporach to the data
# argument
  # hc_object = hclust object
  # leaf_count = maximum number of leaves to investigate 
  # feature_labels = vector of feature data to which the clustering will be analyzed against 

optimizeTree = function (hc_object, leaf_count, feature_labels) {
  # flow control to analyze the cutree for each number of defined leaves (clusters)
  for (i in 2:leaf_count) {
    cut = cutree(hc_object, k = i) # cutree 
    df = analyzeTree(cut, feature_factors = c(levels(feature_labels)), feature_labels = feature_labels) # function generates a breakdown of clustering assignments and feature vaiable per element
    print(df) # displays analysis data strcture to console per cluster count
  }
}
