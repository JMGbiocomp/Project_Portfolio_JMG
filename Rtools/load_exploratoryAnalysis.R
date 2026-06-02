## === Exploratory Analysis Functions === ##
# PackageDependency() uses RCran and Bioconductor repositories to find and install packages 
# Predetermined exploratory analysis packages for R are checked and installed 
# Includes hierarchical clustering, PCA, and 
#

package_list = c("dendextend", "corrplot", "rgl", "PoiClaClu","ggrepel", "ggpubr","umap","Rtsne","dbscan","DESeq2","MASS","apeglm", "ashr")
PackageDependency(package_list)

## Packages
# 1. Base Utilities and Data Handling
library(MASS)
library(PoiClaClu)
library(ashr)
library(apeglm)

# 2. Distance, Clustering & Dimensionality Reduction
library(dbscan)
library(dendextend)
library(Rtsne)
library(umap)

# 3. BioConductor Analysis Tool
library(DESeq2)

# 4. Interactive and Advanced Plotting
library(rgl)
library(corrplot)
library(ggrepel)
library(ggpubr)


## Custom Tools
# source("Rtools/function_.R")
# source("Rtools/function_analysis.R")
# source("Rtools/function_optimize.R")

## Hierarchical Clustering tools
source("Rtools/function_objectHCLUST.R")
source("Rtools/function_analysisCophenetic.R")
source("Rtools/function_analysisHCLUSTCorrelation.R")
source("Rtools/function_analyzeTree.R")
source("Rtools/function_plotPCA.R")

# PCA tools
source("Rtools/function_analysisLoadings.R")
source("Rtools/function_analysisLOD.R")
source("Rtools/function_analysisLOF.R")
source("Rtools/function_analysisMahalanobis.R")
source("Rtools/function_analysisRE.R")
source("Rtools/function_analysisPCA.R")
source("Rtools/function_filterPC.R")
source("Rtools/function_filterPCL.R")

# Varaince Tools
source("Rtools/function_analysisVariance.R")
source("Rtools/function_filterVariance.R")

# Hyper Parameter Tuning
source("Rtools/function_optimizeTree.R")
source("Rtools/function_optimizePCA.R")
source("Rtools/function_optimizeTSNE.R")
source("Rtools/function_optimizeUMAP.R")
source("Rtools/function_optimizeMDS.R")
source("Rtools/function_optimizeDBSCAN.R")
source("Rtools/function_optimizeKmeans.R")

