## === Create a New Feature from existing features from Sample Meta Data === ##
# Function combines two separate features into a single feature label for a data set to faciliate multi-varaible analysis especially for unsupervised learning methods
# arguments:
  # data_object = matrix-like data strucutre for feature meta data with features types as columns and features as rows
  # feature_cols = two element character vector for the two feature type columns in the feature meta data to combine into a single label
# dependencies:
  

codeFeature = function (data_object = NULL, feature_cols = NULL) {
  # errors, warnings, and messages
  if (is.null(data_object)) {stop("must provide a matrix-like data structure with meta data for feature information.")}
  if (is.null(feature_cols)) {stop("must provide a two element character vector for the two feature labels that are to be combined into a new feature label.")}
  
  # flow control to move through each data feature and generate a new feature label
  feature_names = c()
  for (i in 1:dim(data_object)[1]) {
    label_1 = c(data_object[i,feature_cols[1]]) # label 1 from feature 1
    label_2 = c(data_object[i,feature_cols[2]]) # label 2 from feature 2
    new_label = paste0(label_1, "_", label_2) # new label
    feature_names = c(feature_names, new_label)
  }
  return(feature_names) # retunr new feature labels 
}
