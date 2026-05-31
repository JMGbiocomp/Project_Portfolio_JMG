## === Transcripts Per Million Normalization === ##
# Normalization of count data by both gene length (transcripts per kilobases) and sequencing depth
# Transcripts per kilobase (TPR) normalizes data between species (genes)
# Sequencing depth is determined per sample (scale_factor) and used to normalize between sample data
# arguments:
  # count_data = count data matrix (matrix-like) structure with species in rows and samples in columns
  # gene_data = gene meta data with genes in rows and gene information by column
  # col_name = column name containing the gene length (by base)
  # base_unit = number used to convert the gene length value to kilobase; default set to 1000 and used when value is in nucleotide base unit of measure

normTPM = function (count_data = NULL, gene_data = NULL, col_name = NULL, base_unit = 1000, verbose = FALSE) {
  # errors, warnings, and messages
  if (is.null(count_data)) {stop("Must provide a matrix-like data structure of count data with features (samples) in columns and species in rows.")}
  if (is.null(gene_data)) {stop("Must provide a matrix-like data structure of species meta data with features in columns and species in rows.")}
  if (is.null(col_name)) {stop("Must provide a column name within gene_data argument containing the base pair length per species.")}
  if (dim(count_data)[1] != dim(gene_data)[1]) {stop("Species labels and number must between the count_data and gene_data arguments.")}
  if (verbose) {message(paste("Normalization produced is done by a base unit factor or",base_unit))}
  
  # flow control to convert the count data into transcripts per million (TPM)
  new_data = count_data
  for (i in 1:dim(count_data)[1]) {
    new_data[i,] = new_data[i,]/(gene_data[rownames(new_data)[i],col_name]/base_unit) 
  }
  for (c in 1:dim(new_data)[2]) {
    scale_factor = sum(new_data[,c])/1000000
    new_data[,c] = new_data[,c]/scale_factor
  }
  return(new_data)
}

