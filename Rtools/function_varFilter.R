## === Filter species by total variance === ##
# Determines the number of species included if a variance threshold is used for species in an ad hoc analysis
# By default function returns a vector of the indexes for each species meeting the criteria
# arguments:
  # variance_table = data strucutre outputed by the varianceEval function (_x2 dataframe with species_index in column 1 and calculated total varaince by column 2)
  # var_cutoff = single numeric value for the total varaince cutoff by species
  # top_cutoff = logical value to control if a top number of species are to be returned; default set to FALSE
  # top_number = number of species to return as the top number of variance values
  # index_return = lgical value to determine if the vector of indexes is returned; default set to TRUE

varFilter = function (variance_table, var_cutoff = NULL, top_cutoff = FALSE, top_number = 1000, index_return = TRUE) {
  if (top_cutoff == FALSE) {
    indexes = c()
    for (i in 1:dim(variance_table)[1]) {
      if (variance_table[i,"variance"] >= var_cutoff) {
        indexes = c(indexes, variance_table[i,"species_index"])
      }
    }
    print("species count:")
    print(length(indexes))
  } else {
    variance_table = variance_table[1:top_number,]
    indexes = c(t(variance_table[,"species_index"]))
    print("species count:")
    print(top_number)
  }
  if (index_return == TRUE) {
    return(indexes)
  }
}