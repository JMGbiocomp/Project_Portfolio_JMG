##
#
#
#
#
#

analysisCrossValidation = function (data_object = , feature_labels = , training_size = 0.5, sim_ct = 100) {
  # errors, warnings and messages
  
  
  # build results data structure
  summary_data = data.frame(matrix(0, nrow = sim_ct, ncol = ))
  rownames(summary_data) = 1:sim_ct
  colnames(summary_data) = c()
  
  # Split data into training and test sets
  set.seed()
  for(sim in 1:sim_ct) {
    hold_data = as.data.frame(data_object)
    feature_index = 1:dim(hold_data)[2]
    training_index = sample(x = feature_index, size = round(dim(hold_data)[2]*training_size), replace = FALSE)
    test_index = feature_index[-training_index]
    training_data = hold_data[,training_index]
    test_data = hold_data[,test_index]
    
    training_model = regressionModel(model_data = NA, feature_labels = NA, model_formula = y ~ ., glm_family = NA, binarize = FALSE, b_type = c("categories", "success"), binary_cutoff = NA, response_ordered = FALSE, feature_order = NA, hessian = TRUE, model_frame = TRUE)
    
  }
  
  
  
  
  
}