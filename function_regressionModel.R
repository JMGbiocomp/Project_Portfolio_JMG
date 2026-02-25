### === Linear regression modeling === ###
# A function that automates the generation of a linear regression model by evaluating the response variable to determine the most appropriate error distribution and link function
# Functionality includes user override of embedded algorithm to determine the 'family' argument of the glm() function as well as modifying the response variable to becoem binary as either a measure of treatment success or 2-level categorical
  # model_data = independent variable for modeling with samples as columns and species as rows
  # feature_labels = feature variable to be used as response variable for modeling
  # model_formula = formula to build the model; default set to 'y ~ .' to include all species in the model from the data set
  # glm_family = determines the error distribution and link function for the glm() function ('family'); leave as default (NA) to allow the embedded algorithm determine the most appropriate family argument
  # binarize = boulean value to determine if the feature variables will be modified to become binary
  # b_type = determines the type of binary variable; must choose either 'categories' or 'success' 
  # binary_cutoff =. single numeric value vector to use as the cut off value when modifying the feature variable to become binary
library(stats)
regressionModel = function (model_data = NA, feature_labels = NA, model_formula = y ~ ., glm_family = NA, binarize = FALSE, b_type = c("categories", "success"), binary_cutoff = NA,  ) {
  # errors and flags
  if (is.na(model_data)) {stop("requires a data matrix or equivalaent to model")}
  if (is.na(feature_labels)) {stop("requires a data feature as a respoinse variable to model against")}
  if (length(feature_labels) != dim(model_data)[2]) {stop("the number of features as a response variable does not match the sample count (data points) in the model data")}
  if (binarize == TRUE && length(b_type > 1)) {stop("To modify the feature varaible to become binary, the binary type must be specified as either 'categories' or 'success'.")}
  if (binarize == TRUE && is.na(binary_cutoff)) {stop("Must provide a numerical value as a cutoff to change the feature varaible to binary.")}
  if (is.na(glm_family)) {message("The embedded algorithm was used to determien the error distribution and link function for modeling.")}
  
  ## feature variable analysis and preparation ##
  
  feature.ct = length(feature_labels)
  # flow control to change a non-binary variable into a binary variable, either category-based (high vs low) or a success response (1 vs 0) with '1' designating success
  if (binarize == TRUE) {
    for (f in 1:feature.ct) {
      if (feature_labels[f] < binary_cutoff) { # below the cutoff value
        if (b_type == "categories") {
          feature_labels[f] = "low"
        } else if (b_type == "success") {
          feature_labels[f] = 0
        }
      } else { # above the cutoff value
        if (b_type == "categories") {
          feature_labels[f] = "high"
        } else if (b_type == "success") {
          feature_labels[f] = 1
        }
      }
    }
    if (b_type == "categories") {feature_labels = factor(feature_labels, levels = c("low", "high"), labels = "below cutoff", "above cutoff")}
  }
  
  # flow control to determine the correct error distribution and link function for the linear model based on the embedded algorithm for feature detection
  if (is.na(glm_family) == TRUE) {
    # control variables for setting the glm_family argument
    continuous = FALSE
    positive_continuous = FALSE
    count = FALSE
    binary = FALSE
    string = FALSE
    # flow control to evaluate numerical feature variable structure
    for (f in 1:feature.ct) {
      if (is.numeric(feature_labels[f]) == TRUE) {
        if (round(f)!=f) {
          continuous = TRUE # detects non-integer
        } else {
          count = TRUE # detects integer
        }
      } else if (is.character(feature_labels[f])) {
        string = TRUE # detects character
      } else {
        print(f)
        print(feature_labels[f])
        stop("Found feature label above is not a recongized type/class. Review of vector for argument 'feature_labels' is required.")
      }
    }
    if (length(table(feature_labels)) == 2) {binary = TRUE} # determine if feature variable is binary
    if (continuous == TRUE) {
      if (min(feature_labels) >= 0) {positive_continuous = TRUE} # determine if the continuous variable is only with the positve range
    } else if (count == TRUE) {
      if (min(feature_labels) < 0) {continuous = TRUE} # recognize when feature variable is restricted to integers but includes negative values and therefore not count data
    }
    # set glm_family variable
    if (count == TRUE && continuous == FALSE & binary == FALSE && string == FALSE) {
      glm_family = poisson() # recognized as count data
    } else if (binary == TRUE && string == FALSE) {
      glm_family = binomial() # recognized binomial data as a success response variable
    } else if (positive_continuous == TRUE && string == FALSE) {
      glm_family = Gamma() # recognized as continuous variable feature that is always positive and skewed
    } else if (continuous == TRUE && string == FALSE) {
      glm_family = gaussian() # recognized as a true continuous variable that follows a normal distribution
    } else if (binary == TRUE && string == TRUE) {
      glm_family = binomial # recognized binomial data as a 2-level category variable
      feature_labels = as.factor(feature_labels)
    } else {
      stop("Error distributiona and link function cannot be determined by embedded algorithm. Mannually adjust feature variable to desired structure or set 'glm_family' argument.")
    }
  }
  y = feature_labels # sets the feature varaible as the dependent response variable 
  model = glm(formula = model_formula, family = glm_family, data = t(model_data)) # generate the generalized linear model to the determined specs
  return(model)
}