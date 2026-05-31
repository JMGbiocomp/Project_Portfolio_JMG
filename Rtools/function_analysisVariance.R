### === Analysis of Variance Stability === ###
# Function to provide a table or visualization of the variance across all species in a data set
# Useful in determining how well variance has been stabilized between species from normalization methods across samples 
# arguments:
  # data_object = matrix-like data structure with features as columns and species as rows
  # plot_title = character vector used as the title of the various visualizations generated
  # data_return = logical value to determine if fucntion retunrs the summary statistics table 
  # hist_return = logical value to determine if a histogram of the variances is returned 
  # scatter_return = logical value to determine if a scatter plot of the variances by species index is returned 
  # MSD = logical value to determine if a Mean-SD plot is returned 
# dependencies:
  # stats, ggplot2

analysisVariance = function (data_object, plot_title, data_return = "none", histogram = TRUE, scatter = TRUE, meanSD = TRUE) {
  
  variance_data = data.frame(matrix(0, nrow = dim(data_object)[1], ncol = 4))
  colnames(variance_data) = c("index","mean","std","variance")
  rownames(variance_data) = rownames(data_object)
  variance_data$index = 1:dim(data_object)[1]
  
  for (species in 1:dim(data_object)[1]) {
    variance_data[species,"mean"] = mean(c(t(data_object[species,])))
    variance_data[species,"std"] = sd(c(t(data_object[species,])))
    variance_data[species,"variance"] = var(c(t(data_object[species,])))
  }
  if (histogram) {
    hist(variance_data[,"variance"], main = plot_title, xlab = "variance")
  }
  if (scatter) {
    plot(x = variance_data$index, y = variance_data$variance, main = plot_title, xlab = "species index", ylab = "variance")
  }
  
  mds_plot = ggplot2::ggplot(data = variance_data, aes(x = mean, y = std)) + geom_point(color = "blue", size = 1) + labs(title = plot_title, x = "mean", y = "std") + geom_smooth(method="gam", col = "red", linewidth = 0.3, se = FALSE) + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  if (meanSD) {
    print(mds_plot)
  }
   if (data_return == "summary") {
    return(variance_data)
   }
}
