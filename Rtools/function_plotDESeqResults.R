##
#
#
#
#

plotDESeqResults = function (data_object = NULL, plot_title = NULL, shrink_data = FALSE, top_labels = 10, FC_threshold = 1.2, PV_threshold = 0.01, stat_threshold = 1.5) {
  #
  if (is.null(data_object)) {stop()}
  if (is.null(plot_title)) {plot_title = "Generic Results Title"}
  # if(mode(data_object) != "S4") {stop("Input data object must be an S4-class object from the results of a DESeq analysis.")}
  
  
  #
  hold_data = as.data.frame(data_object)
  rownames(hold_data) = rownames(data_object)
  colnames(hold_data) = colnames(data_object)
  hold_data$species = rownames(data_object)
  
  p4 = ggplot(data = hold_data, aes(x = pvalue)) + geom_histogram() + coord_cartesian(xlim = c(0, 1)) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") 
  
  hold_data$pvalue = -log10(hold_data$pvalue)
  hold_data$padj = -log10(hold_data$padj)
  PV_threshold = -log10(PV_threshold)
  
  filter_SE = hold_data
  high_SE = filter_SE[abs(filter_SE$log2FoldChange)-filter_SE$lfcSE <= 1.1,]
  filter_SE = filter_SE[abs(filter_SE$log2FoldChange)-filter_SE$lfcSE > 1.1,]
  
  
  
  # Subset Data
  top_FC = filter_SE
  top_FC$log2FoldChange = abs(top_FC$log2FoldChange)
  top_FC = top_FC[order(top_FC$log2FoldChange, decreasing = TRUE),]
  top_FC = top_FC[1:top_labels,]
  top_FC = hold_data[rownames(top_FC),]
  
  if (shrink_data == FALSE) {
    top_stat = filter_SE
    top_stat$stat = abs(top_stat$stat)
    top_stat = top_FC[order(top_stat$stat, decreasing = TRUE),]
    top_stat = top_stat[1:top_labels,]
    top_stat = hold_data[rownames(top_stat),]
  }
  
  
  # 
  if (shrink_data == FALSE) {
    p1 = ggplot(data = hold_data, aes(x = stat, y = padj)) + geom_point(color = "grey") + geom_point(data = subset(hold_data, abs(stat) > stat_threshold & padj < PV_threshold), color = "green") + geom_point(data = subset(hold_data, abs(stat) < stat_threshold & padj > PV_threshold), color = "blue") + geom_point(data = subset(hold_data, abs(stat) > stat_threshold & padj > PV_threshold), color = "red") + geom_text(data = top_stat, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  } else {
    p1 = ggplot(data = hold_data, aes(x = log2FoldChange, y = lfcSE)) + geom_point(color = "grey") + geom_point(data = high_SE, color = "red") + geom_text(data = filter_SE, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  }
  p2 = ggplot(data = hold_data, aes(x = log2FoldChange, y = padj)) + geom_point(color = "grey") + geom_point(data = subset(hold_data, abs(log2FoldChange) > FC_threshold & padj < PV_threshold), color = "green") + geom_point(data = subset(hold_data, abs(log2FoldChange) < FC_threshold & padj > PV_threshold), color = "blue") + geom_point(data = subset(hold_data, abs(log2FoldChange) > FC_threshold & padj > PV_threshold), color = "red") + geom_text(data = top_FC, aes(label = species, vjust = -1), size = 2) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none")
  p3 = ggplot(data = hold_data, aes(x = baseMean, y = log2FoldChange)) + geom_point(color = "grey") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") 
  
  
  
    
  gridExtra::grid.arrange(grobs = list(p1,p2,p3,p4), nrow = 2, ncol = 2, top = plot_title)
}

