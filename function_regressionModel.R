### === Linear regression modeling === ###
# A function that automates the generation of a linear regression model by evaluating the response variable to determine the most appropriate error distribution and link function
# Functionality includes user override of embedded algorithm to determine the 'family' argument of the glm() function, modifying the response variable to become binary as either a measure of treatment success or 2-level categorical, and modeling of categorical response varaibles with 3 or more levels (nomial and ordinal)
  # model_data = independent variable for modeling with samples as columns and species as rows
  # feature_labels = feature variable to be used as response variable for modeling
  # model_formula = formula to build the model; default set to 'y ~ .' to include all species in the model from the data set
  # glm_family = determines the error distribution and link function for the glm() function ('family'); leave as default (NA) to allow the embedded algorithm determine the most appropriate family argument
  # binarize = logical value to determine if the feature variables will be modified to become binary
  # b_type = determines the type of binary variable; must choose either 'categories' or 'success' in which the response variable is converted into a 2-level categorical variable (high vs low) or representative of treatment success with 1 designating a successful response, respectively
  # binary_cutoff = single numeric value vector to use as the cut off value when modifying the feature variable to become binary
  # response_ordered = logical value to indicate that the categorical feature variable (response) is unordered (nominal) or ordered (ordinal), which helps defines the regression model function and factorization of the response variable
  # feature_order = character vector that defines the order of factors for the feature (response) variable 
  # hessian = logical for whether the Hessian (the observed/expected information matrix) should be returned; default set to TRUE
  # model_frame = a logical value indicating whether model frame should be included as a component of the returned value; default set to TRUE

library(stats); library(nnet); library(MASS)
regressionModel = function (model_data = NA, feature_labels = NA, model_formula = y ~ ., glm_family = NA, binarize = FALSE, b_type = c("categories", "success"), binary_cutoff = NA, response_ordered = FALSE, feature_order = NA, hessian = TRUE, model_frame = TRUE) {
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
    } else if (binary == TRUE && string == TRUE && length(unique(feature_labels))<3) {
      glm_family = binomial # recognized binomial data as a 2-level category variable
      if ("low" %in% feature_labels) {
        feature_labels = as.factor(feature_labels, levels = c("low", "high"))
      } else {
        feature_labels = as.factor(feature_labels)
      }
    } else if (binary == FALSE && string == TRUE && length(unique(feature_labels))<2) { # recognized feature data as a 3 or more level categorical variable 
      glm_family = "multi-level"
    } else {
      stop("Error distribution and link function cannot be determined by embedded algorithm. Mannually adjust feature variable to desired structure and type or set 'glm_family' argument.")
    }
  }
  if (glm_family == "multi-level") {
    # flow control to build a model with the feature (response) variable as ordinal or multinomical 
    if (response_ordered == TRUE) {
      feature_labels = ordered(feature_labels, levels = feature_order) # ordered factorization
      y = feature_labels
      model = polr(formula = model_formula, data = t(model_data), model = model_frame, Hess = hessian) # default is set to include Hessian matrix and model frame are included in the model object
    } else {
      feature_labels = as.factor(feature_labels) # nomial factorization
      y = feature_labels
      model = multinom(formula = model_formula, data = model_data, model = model_frame, Hess = hessian) # default is set to include Hessian matrix and model frame are included in the model object
    }
  } else {
    y = feature_labels # sets the feature variable as the dependent response variable 
    model = glm(formula = model_formula, family = glm_family, data = t(model_data), model = model_frame) # generate the generalized linear model to the determined specs
  }
  
  return(model)
}