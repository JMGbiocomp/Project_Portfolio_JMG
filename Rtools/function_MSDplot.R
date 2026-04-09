## === Variance vs Mean Ploting === ##
# Plot the standard deviation against the mean across all species within a data set
# Intended QC function as part of the variance stability evaluation pipeline for count data
# arguments:
  # variance_table = data structure outputted by the varianceEval function that has species index, total mean, and variance columns for all gene species
  # plot_title = main visualization title for output graphic
# dependencies
  # ggplot
  # gridExtras

MSDplot = function (variance_table, plot_title) {
  variance_table[,"variance"] = sqrt(variance_table[,"variance"])
  ggplot(data = variance_table, aes(x = mean, y = variance)) + geom_point(color = "blue", size = 1) + labs(title = plot_title, x = "Mean", y = "Variance") + geom_smooth(method="gam", col = "red", size = 0.3, se = FALSE) + theme(panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
}
