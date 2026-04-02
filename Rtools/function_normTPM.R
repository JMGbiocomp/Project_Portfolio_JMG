## === Transcripts Per Million Normalization === ##
# Normalization of count data by both gene length (transcripts per kilobases) and sequencing depth
# Transcripts per kilobase (TPR) normalizes data between species (genes)
# Sequencing depth is determined per sample (scale_factor) and used to normalize between sample data
# arguments:
  # count_data = count data matrix (matrix-like) structure with species in rows and samples in columns
  # gene_data = gene meta data with genes in rows and gene information by column
  # col_name = column name containing the gene length (by base)
  # base_unit = number used to convert the gene length value to kilobase; default set to 1000 and used when value is in nucleotide base unit of measure

normTPM = function (count_data, gene_data, col_name, base_unit = 1000) {
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

