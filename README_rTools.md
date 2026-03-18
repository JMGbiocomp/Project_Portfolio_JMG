# R Cran Tools (Functions) README File

Description:
A collection of R functions designed to handle, process, analyze, visualize and (synthetically) generate large biological data.  Functions leverage R packages from CRAN and Bioconductor repositories to offer streamlined and automated tasks common in bioinformatic and computational pipelines as well as provide in-depth functionality with user-level access to algorithmic parameters for fine tuning functions.  An overview of each function is provided here within the README file with in-depth documentation located within the specific function's script file that explains each argument, usage, and limitations.  README file is intended to understand the collection as a whole and guide users to functions of interest.  

## Utilities

Description:

Tool List:
- function_PackageDependency.R

### Function Descriptions



## Data Processing Tools

Description:

Tools List:
- 
- 
- 
- 

### Function Descriptions



## Statistical Analysis Tools

Description:

Tools List:
- function_hclusteringObject.R
- function_copheneticEval.R 
- function_linkageCorrelation.R
- function_regressionModel.R
- function_varianceEval.R
- function_plotPCA.R

### Function Descriptions

hclusteringObject():
Generates a heirarchical clustering object from a dataframe or like-formated data set for performing unsupervised learning analysis methods. 

copheneticEval():
Generates a dataframe with the cophenetic distances for each distance calcualtion and linkage method combination.  This summary table of cophenetic distacnes is an unsupervised learning tool to help determine the best distance calculation and linkage method for generating the heirarchical clustering object of a data set.  

linkageCorrelation():
Generates and plots the correaltion matrix from a heirarchical clustering object.  A useful tool to evaluate all linkage methods for a specified distance calculations for optimizing heriarchical clustering.  

regressionModel():
An automated linear regression model builder capable of classifying the response variable and determining the corresponding errro distribution and link function for constructing the model.  Also, it allows a manual override of the automated algorithm for user-level control.

varianceEval():
Determines and visualizes the variance accross all samples within a data set with an option to output a generated dataframe with the variances.  This tool is valuable for determining how well variance has been stablized within the data set and guide normalization methods.  

plotPCA():


## Data Visualization Tools

Description:

Tools List:
- 
- 
- 
- 

### Function Descriptions


## Generating Synthetic Data Sets

Description:

Tools List:
- function_RNAseqSim.R
- function_RNAseqSim2.R
- 
- 

### Function Descriptions

