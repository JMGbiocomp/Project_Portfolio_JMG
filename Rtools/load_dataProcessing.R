## === Sourcing data processing, normalization and filtering functions and loading their dependencies for RNAseq analysis === ##
#
#
#
#


package_list = c("ggplot2", "gridExtra","data.table")
PackageDependency(package_list)
# packages
library("ggplot2", "gridExtra","data.table")

# tools
source("Rtools/function_IndexDuplication.R")
source("Rtools/function_normTPM.R")
source("Rtools/function_varianceEval.R")
source("Rtools/function_varFilter.R")
source("Rtools/function_MSDplot.R")