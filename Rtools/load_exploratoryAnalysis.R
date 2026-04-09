## === Exploratory Analysis Functions === ##
# PackageDependency() uses RCran and Bioconductor repositories to find and install packages 
# Predetermined exploratory analysis packages for R are checked and installed 
# Includes heirarchical clustering, PCA, and 
#

package_list = c("dendextend", "corrplot", "rgl", "PoiClaClu","ggrepel", "ggpubr","umap","Rtsne")
PackageDependency(package_list)
library("dendextend")
library("corrplot") 
library("rgl")
library("PoiClaClu")
library("ggrepel")
library("ggpubr")
library("umap")
library("Rtsne")
library()
library()

source("Rtools/function_hclusteringObject.R")
source("Rtools/function_linkageCorrelation.R")
source("Rtools/function_copheneticEval.R")
source("Rtools/function_plotPCA.R")
source("Rtools/function_ggplotPCA.R")
source("Rtools/function_analyzeTree.R")
source("Rtools/function_optimizeTree.R")
source("Rtools/function_sAnalyzePCA.R")
source("Rtools/function_kclusterOptimize.R")
source("Rtools/function_kclusterWithinSS.R")
source("Rtools/function_clusterAnalysis.R")
source("Rtools/function_umapOptimize.R")
source("Rtools/function_tsneOptimize.R")