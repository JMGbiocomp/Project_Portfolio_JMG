## === Identify Indexes of DUplicated Elements === ##
# Find duplicate indexes in a data structure column
# IDeal for evaluating duplciates and removing them from a data structure 
# argument:
  # input_data  = data strucutre with meta data in columns and species in rows - i.e. genes in rows and gene symbols and info in columns
  # col_name = column name to chekc for duplicate entries

IndexDuplication = function (input_data, col_name) {
  query_values = c()
  duplicated_index = c()
  for (i in 1:dim(input_data)[1]) {
    if (input_data[i,col_name] %in% query_values) {
      duplicated_index = c(duplicated_index, i)
    } else {
      query_values = c(query_values, input_data[i,col_name])
    }
  }
  return(duplicated_index)
}
