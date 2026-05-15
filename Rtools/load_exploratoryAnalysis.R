## === Exploratory Analysis Functions === ##
# PackageDependency() uses RCran and Bioconductor repositories to find and install packages 
# Predetermined exploratory analysis packages for R are checked and installed 
# Includes hierarchical clustering, PCA, and 
#

package_list = c("dendextend", "corrplot", "rgl", "PoiClaClu","ggrepel", "ggpubr","umap","Rtsne","dbscan")
PackageDependency(package_list)
# packages
library("dendextend")
library("corrplot") 
library("rgl")
library("PoiClaClu")
library("ggrepel")
library("ggpubr")
library("umap")
library("Rtsne")
library("dbscan")
#library()

# tools
# source("Rtools/function_.R")
# source("Rtools/function_analysis.R")
# source("Rtools/function_optimize.R")
source("Rtools/function_objectHCLUST.R")
source("Rtools/function_analysisCophenetic.R")
source("Rtools/function_analysisHCLUSTCorrelation.R")
source("Rtools/function_analysisLoadings.R")
source("Rtools/function_analysisLOD.R")
source("Rtools/function_analysisLOF.R")
source("Rtools/function_analysisMahalanobis.R")
source("Rtools/function_analysisPCA.R")
source("Rtools/function_analysisRE.R")
source("Rtools/function_analysisVariance.R")
source("Rtools/function_analysisTree.R")
source("Rtools/function_filterPC.R")
source("Rtools/function_filterPCL.R")
source("Rtools/function_filterVariance.R")
source("Rtools/function_optimizeTree.R")
source("Rtools/function_optimizePCA.R")
source("Rtools/function_optimizeTSNE.R")
source("Rtools/function_optimizeUMAP.R")
source("Rtools/function_optimizeMDS.R")
source("Rtools/function_optimizeDBSCAN.R")
source("Rtools/function_optimizeKmeans.R")
source("Rtools/function_plotPCA.R")