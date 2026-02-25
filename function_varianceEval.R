### === Variance Evaluation of data set === ###
# Function to provide a table or visualization of the varaince across all species in a data set
# Useful in determining how well variance has been stabilized between species from normalization methods across samples 
# Arguments:
  # count_data = the count matrix for analysis with samples as columns and species as rows; functionality extends beyond count data and works with other samples from different distribution types
  # plot_title = character vector used as the title of the visualizations
  # table_return = boulean value to determine if fucntion retunrs the results as a table
  # hist_return = boulean value to determine if a histogram of the variances is returned 
  # scatter_return = boulean value to determine if a scatter plot of the variances by species index is returned 

varianceEval = function (count_data, plot_title = NULL, table_return = FALSE, hist_return = TRUE, scatter_return = FALSE) {
  species.ct = dim(count_data)[1] # number of species
  temp.df = data.frame(matrix(0, nrow = species.ct, ncol = 2)) # data frame for building visualizations and retunr if table_return = TRUE
  colnames(temp.df) = c("species_index", "variance") # set column names for data frame
  temp.df[,"species_index"] = 1:species.ct # fill species index values in column 1
  # flow control to determine variance of each species across samples and what will be returned as output of the function by arguments
  for (species in 1:species.ct) {
    temp.df[species, "variance"] = var(c(t(count_data[species,])))
  }
  # return histogram
  if (hist_return == TRUE) {
    hist(temp.df[,"variance"], main = plot_title, xlab = "variance")
  }
  # return scatter plot
  if (scatter_return == TRUE) {
    plot(x = temp.df$species_index, y = temp.df$variance, main = plot_title, xlab = "species index", ylab = "variance")
  }
  # return table
  if (table_return == TRUE) {
    return(temp.df)
  }
}