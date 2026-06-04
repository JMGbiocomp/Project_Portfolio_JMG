## === Generate VIsualization for Validating and Analyzing DEG Results from DESeq === ##
# Function creates four plots depending on if the LFC data underwent shrinking:
  # Test statistic vs Log2FoldChange plot (no shrinking):
                    # or
  # LogFoldChange Standard Error vs Log2FoldChange plot: 
  # Adjusted p-value vs Log2FoldChange plot:
  # Log2FoldChange vs baseMean plot:
  # p-value histogram: 
# arguments:
  # data_object = an S4 object created from the result() function from a DESeq object or equvalent matrix-like data structure (column names are the same)
  # plot_title = character to use for the plot title 
  # shrink_data = logical vector to determine if the data_object's LFC is shrunk
  # top_labels = the number of top species to label in the plots
  # FC_threshold = log2FoldChange threshold cutoff
  # PV_threshold = p-value threshold cutoff
  # stat_threshold = p-test statistic threshold cutoff
  # return_data = logical threshold to determine if the top gene species identified are returned
# dependencies
  # ggplot2, gridExtra, stat, DESeq2

plotDESeqResults = function (data_object = NULL, plot_title = NULL, shrink_data = FALSE, top_labels = 10, FC_threshold = 1.2, PV_threshold = 0.01, stat_threshold = 1.5, return_data = FALSE) {
  # Errors, warnings, and messages
  if (is.null(data_object)) {stop()}
  if (is.null(plot_title)) {plot_title = "Generic Results Title"}
  if(mode(data_object) != "S4") {
    warning("Data object is not an S4-class object from the results of a DESeq analysis")
  } else {
    message("Analyzing DEG results from S4 data object.")
  }
  
  
  # Convert S4 object into a data frame for performing calculations and plotting
  hold_data = as.data.frame(data_object)
  rownames(hold_data) = rownames(data_object)
  colnames(hold_data) = colnames(data_object)
  hold_data$species = rownames(data_object) # add species names into data for convenience 

  # transform adjusted p-vlaues for plotting
  hold_data$padj = -log10(hold_data$padj)
  PV_threshold = -log10(PV_threshold) # transfrom threshold 
  
  # subset data for SE labeling 
  filter_SE = hold_data
  high_SE = filter_SE[abs(filter_SE$log2FoldChange)-filter_SE$lfcSE <= 1.0,]
  filter_SE = filter_SE[abs(filter_SE$log2FoldChange)-filter_SE$lfcSE > 1.0,] # only species with log2FoldChange-SE > 1 remain
  
  # Subset Data for labeling for fold change and statistic metrics after filtered for SE
  top_FC = filter_SE
  top_FC$log2FoldChange = abs(top_FC$log2FoldChange)
  top_FC = top_FC[order(top_FC$log2FoldChange, decreasing = TRUE),]
  top_FC = top_FC[1:top_labels,] # top # species only
  top_FC = hold_data[rownames(top_FC),]
  
  if (shrink_data == FALSE) {
    top_stat = filter_SE
    top_stat$stat = abs(top_stat$stat)
    top_stat = top_FC[order(top_stat$stat, decreasing = TRUE),]
    top_stat = top_stat[1:top_labels,] # top # species only
    top_stat = hold_data[rownames(top_stat),]
  }
  
  
  # generate plots for results
  if (shrink_data == FALSE) {
    p1 = ggplot(data = hold_data, aes(x = stat, y = padj)) + geom_point(color = "grey") + geom_point(data = subset(hold_data, abs(stat) > stat_threshold & padj < PV_threshold), color = "green") + geom_point(data = subset(hold_data, abs(stat) < stat_threshold & padj > PV_threshold), color = "blue") + geom_point(data = subset(hold_data, abs(stat) > stat_threshold & padj > PV_threshold), color = "red") + geom_text(data = top_stat, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  } else {
    p1 = ggplot(data = hold_data, aes(x = log2FoldChange, y = lfcSE)) + geom_point(color = "red") + geom_point(data = high_SE, color = "grey") + geom_text(data = filter_SE, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  }
  p2 = ggplot(data = hold_data, aes(x = log2FoldChange, y = padj)) + geom_point(color = "grey") + geom_point(data = subset(hold_data, abs(log2FoldChange) > FC_threshold & padj < PV_threshold), color = "green") + geom_point(data = subset(hold_data, abs(log2FoldChange) < FC_threshold & padj > PV_threshold), color = "blue") + geom_point(data = subset(hold_data, abs(log2FoldChange) > FC_threshold & padj > PV_threshold), color = "red") + geom_text(data = top_FC, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  p3 = ggplot(data = hold_data, aes(x = baseMean, y = log2FoldChange)) + geom_point(color = "grey") + scale_x_log10() + geom_point(data = subset(hold_data, abs(log2FoldChange) > FC_threshold), color = "red") + geom_text(data = top_FC, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") 
  p4 = ggplot(data = hold_data, aes(x = pvalue)) + geom_histogram() + coord_cartesian(xlim = c(0, 1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") 
  
  gridExtra::grid.arrange(grobs = list(p1,p2,p3,p4), nrow = 2, ncol = 2, top = plot_title) # returns them in A 2x2 grid visual
  
  if (return_data) {
    return(rownames(top_FC))
  }
}

