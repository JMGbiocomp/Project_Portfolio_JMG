## === Creates a Symbol Scheme for Values/Features Intended for Plots === ##
# Function with a predefined set of plotting symbols that are distinct from one another to sample from
# Functionality includes handles when the sample size is larger than the different distinct symbols
# arguments:
  # feature_data = character or numeric vector for feature labels
  # color_replace = override control for replacement function of sampling

codeSymbol = function (feature_labels, symbol_replace = FALSE) {
  factors = unique(feature_labels)
  
  
  if (length(factors) <= 12) {
    symbol_values = c(0,1,2,3,4,5,8,9,15,16,17,18)
    symbol_code = sample(symbol_values, size = length(factors), replace = symbol_replace)
  } else if (length(factors) <= 26) {
    symbol_values = c(0:25)
    symbol_code = sample(symbol_values, size = length(factors), replace = symbol_replace)
  } else {
    symbol_values = c(0:25)
    symbol_code = sample(symbol_values, size = length(factors), replace = TRUE)
  }
  
  symbol_match = c()
  for (i in 1:length(feature_labels)) {
    for (s in 1:length(symbol_code)) {
      if (feature_labels[i] == factors[s]) {
        symbol_match = c(symbol_match, symbol_code[s])
      }
    }
  }
  return(symbol_match)
}
