## === Filter Species by CPM Normalization === ## 
#
#
#
#
# arguments:
  # data_object = 
  # feature_labels = 
# dependencies:
  # 

filterCPM = function (data_object, feature_labels) {
  cpm_object = normCPM(data_object = data_object) # CPM normalized data
  cpm_threshold = min(table(feature_labels)) # dynamic threshold fitted to lowest experimental group size
  keep_index = c()
  for (species in 1:dim(cpm_object)[1]) {
    pass_count = 0
    for (sample in 1:dim(cpm_object)[2]) {
      if (cpm_object[species,sample] >= 1) {
        pass_count = pass_count + 1
      }
    }
    if (pass_count >= cpm_threshold) {
      keep_index = c(keep_index, species)
    }
  }
  return(data_object[keep_index,])
}
