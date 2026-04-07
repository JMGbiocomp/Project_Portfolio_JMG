## === Exploratory Analysis Functions === ##
# PackageDependency() uses RCran and Bioconductor repositories to find and install packages 
# Predetermined exploratory analysis packages for R are checked and installed 
# Includes heirarchical clustering, PCA, and 
#

package_list = c("dendextend", "corrplot", "rgl", "PoiClaClu")
PackageDependency(package_list)
library("dendextend")
library("corrplot") 
library("rgl")
library("PoiClaClu")

source("Rtools/function_hclusteringObject.R")
source("Rtools/function_linkageCorrelation.R")
source("Rtools/function_copheneticEval.R")
source("Rtools/function_plotPCA.R")
source("Rtools/function_analyzeTree.R")
source("Rtools/function_sAnalyzePCA.R")
source("Rtools/function_kmeansAnalysis.R")
source("Rtools/function_withinSSanalysis.R")