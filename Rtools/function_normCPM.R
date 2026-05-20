## === Counts per Million Normalization === ##
# Function that normalizes the count data by sequencing depth per sample
# arguments:
  # data_object = a matrix-like data structure of raw count data from high-throughput sequencing

normCPM = function (data_object) {
  
  cpm_object = data.frame(matrix(0,nrow=dim(data_object)[1],ncol=dim(data_object)[2])) # define normalized data object structure
  rownames(cpm_object) = rownames(data_object) # copy species names
  colnames(cpm_object) = colnames(data_object)# copy sample names
  seq_depth = colSums(data_object) # sequencing depth by sample
  # flow control to calculate CPM per raw count
  for (sample in 1:dim(data_object)[2]) {
    sample_depth = seq_depth[sample]
    cpm_object[,sample] = data_object[,sample]/sample_depth*1000000
  }
  return(cpm_object) # return normalized data
}
