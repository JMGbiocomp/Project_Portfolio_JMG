### === 2D visualizations with ggplot2 of principal components (highest covariance) within a data set using ggplot2 after genes filtered by PC loading === ###
# Funciton takes two pca objects (or one data set), one run by features (samples) and species (genes), finds the species with the largest contribution (load) to the covariance within the data set and generates a visualization
# Ideal to filter out unimportant genes to the major PC in the pca object and find the most significant impact on the covariance within the data
# arguments:
  # count_data = count data ideally transformed and/or normalized
  # species_labels = species names for the corresponding count data or pca objects
  # main_title = name of the returned plot with all three comparisons of the top PCs
  # pca_1 = pca object 1 by feature
  # pca_2 = pca object 2 by species 
  # linkage_method = linkage method for creating pca objects
  # dist_calc = distance calculation method for creating pca objects
  # input_type = data input type either count data or pca objects; default set to 'count'
# dependencies
  # stats
  # dendextend
  # ggplot2
  # gridExtras


sAnalyzePCA = function (count_data = NA, species_labels = NA, main_title, input_type = "count", pcaData_1, pcaData2, cutoff_overide = FALSE, cutoff = 1.1, visualize = FALSE, visual_count = 10, return = FALSE) {
  # flow control to generate and/or define pca objects
  if (input_type == "count") {
    pca_feature = prcomp(x = (t(count_data)), scale = FALSE)
    pca_species = prcomp(x = count_data, scale = FALSE)
  } else if (input_type == "pca") {
    pca_feature = pcaData_1
    pca_species = pcaData2
  } else {stop("Must specify input data type as either 'count' or 'pca'")}
  
  # find species with a significant contritbution to the top three PCs
  load_sig = sqrt(1/(length(species_labels))) # caculated value if all species have equal contribution to PC covariance 
  top_genes = c() # holds all species names found
  v_names = c()
  # flow control to find all the top species 
  for (i in 1:3) {
    gene_load = pca_feature$rotation
    if (cutoff_overide == TRUE) {
      indices <- which(gene_load[, i] > cutoff)
    } else {
      indices <- which(gene_load[, i] > load_sig*cutoff)
    }
    gene_names = rownames(gene_load)[indices]
    if (length(gene_names) >= visual_count) {
      v_names = c(v_names, gene_names[1:visual_count])
    } else {
      v_names =c(v_names, gene_names)
    }
    top_genes = c(top_genes, gene_names)
  }
  # filter out the species that do not significantly contribute to the PCs covariance 
  top_loaders = pca_species$x
  if (input_type == "count") {
    rownames(top_loaders) = rownames(count_data)
  } else if (input_type == "pca") {
    rownames(top_loaders) = species_labels
  }
  v_names = unique(v_names)
  top_genes = unique(top_genes)
  top_loaders = top_loaders[top_genes,]
  groups = rownames(top_loaders) # get all top contributing gene names
  colors = sample(x = colors(distinct = FALSE), size = length(top_genes), replace = TRUE) # color code vector to designate feature labels by color
  point_colors = colors[match(top_genes, groups)]
  
  # create 2D plots for PCA
  if (visualize == TRUE) {
    v_colors = rainbow(length(v_names))
    # PC1 vs PC2
    p1 = ggplot(data = top_loaders, aes(x = PC1, y = PC2)) + geom_point(color = "grey", size = 1, show.legend = FALSE) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1) + geom_text_repel(data = top_loaders[v_names,], aes(label = v_names), vjust = -1, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #p1 = ggplot(data = top_loaders, aes(x = PC1, y = PC2)) + geom_point(color = "grey", size = 1.5) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1.5, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #ggplot(data = top_loaders, aes(x = PC1, y = PC2)) + geom_point(color = "grey", size = 1) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1, show.legend = TRUE)
    # PC1 vs PC3
    p2 = ggplot(data = top_loaders, aes(x = PC1, y = PC3)) + geom_point(color = "grey", size = 1, show.legend = FALSE) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1) + geom_text_repel(data = top_loaders[v_names,], aes(label = v_names), vjust = -1, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #p2 = ggplot(data = top_loaders, aes(x = PC1, y = PC3)) + geom_point(color = "grey", size = 1.5) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1.5, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #ggplot(data = top_loaders, aes(x = PC1, y = PC3)) + geom_point(color = "grey", size = 1) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1, show.legend = TRUE)
    # PC2 vs PC3
    p3 = ggplot(data = top_loaders, aes(x = PC2, y = PC3)) + geom_point(color = "grey", size = 1, show.legend = FALSE) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1) + geom_text_repel(data = top_loaders[v_names,], aes(label = v_names), vjust = -1, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #p3 = ggplot(data = top_loaders, aes(x = PC2, y = PC3)) + geom_point(color = "grey", size = 1.5) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1.5, show.legend = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "bottom")
    #ggplot(data = top_loaders, aes(x = PC2, y = PC3)) + geom_point(color = "grey", size = 1) + geom_point(data = top_loaders[v_names,], color = v_colors, size = 1, show.legend = TRUE)
    #grid
    gridExtra::grid.arrange(grobs = list(p1, p2, p3), nrow = 1, ncol = 3, top = main_title)
  } else {
    # PC1 vs PC2
    p1 = ggplot(data = top_loaders, aes(x = PC1, y = PC2, color = point_colors))+ geom_point(size = 1.5) + scale_color_manual(values = colors) + labs(x = "PC1", y = "PC2") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") # Optional: adds axis lines back)
    # PC1 vs PC3
    p2 = ggplot(data = top_loaders, aes(x = PC1, y = PC3, color = point_colors)) + geom_point(size = 1.5) + scale_color_manual(values = colors) + labs(x = "PC1", y = "PC3") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") # Optional: adds axis lines back)
    # PC2 vs PC3
    p3 = ggplot(data = top_loaders, aes(x = PC2, y = PC3, color = point_colors)) + geom_point(size = 1.5) + scale_color_manual(values = colors)  + labs(x = "PC2", y = "PC3")+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.position = "none") # Optional: adds axis lines back)
    # grid
    gridExtra::grid.arrange(grobs = list(p1, p2, p3), nrow = 1, ncol = 3, top = main_title)
  }
  
  if (return == "PC") {
    return(top_loaders)
  } else if (return == "genes") {
    return(v_names)
  } else if (return == FALSE) {
    print("PC loading analysis complete without return")
  }
}
