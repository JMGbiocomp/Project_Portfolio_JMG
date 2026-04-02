## === Analyzing clustering of features within branches of hierarchical clustering === ##
# A cutree() object is analayzed against vector of features to evaluate how well hierarchical clsutering captures these features (biology in question)
# arguments
  # cut_tree = output from the cutree() function that labels leaves by the branches determiend by the cut
  # feature_factors = vector of unique values within the feature_label vector
  # feature_labels = vector of features from meta data that matches the feature order (i.e. samples)
# dependencies:
  # dendextend

analyzeTree = function (cut_tree, feature_factors, feature_labels) {
  leaf.ct = length(cut_tree)
  branch.ct = length(table(cut_tree))
  feature.ct = length(feature_factors)
  output_df = data.frame(matrix(0, nrow = branch.ct, ncol = feature.ct + 1))
  colnames(output_df) = c("total count", feature_factors)
  for (b in 1:branch.ct) {
    branch_total = 0
    for (i in 1:leaf.ct) {
      if (cut_tree[i] == b) {
        branch_total = branch_total + 1
        for (f in 1:feature.ct) {
          if (feature_labels[i] == feature_factors[f]) {
            output_df[b,(f+1)] = output_df[b,(f+1)] + 1
          }
        }
      }
    }
    output_df[b,1] = branch_total
  }
  return(output_df)
}
