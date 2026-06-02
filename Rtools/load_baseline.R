## === Sourcing function to handle package dependencies and loading baseline packages === ##
# PackageDependency() uses RCran and Bioconductor repositories to find and install packages 
# Predetermined baseline packages for R are checked and installed 
# Recommended that this script file is sourced at the beginning of any script and allows sourcing of other "download_*" files within scripts
# No arguments needed 

source("Rtools/function_PackageDependency.R")
package_list = c("stats","Biobase", "generics", "BioGenerics", "SummarizedExperiment", "R.utils")
PackageDependency(package_list)


library(SummarizedExperiment)
library(Biobase)
library(R.utils)
library(generics)
library(BiocGenerics)
library(stats)
