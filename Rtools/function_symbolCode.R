## === Creates a Symbol Scheme for Values/Features Intended for Plots === ##
#
#
# arguments:
  # feature_labels
  # symbol_replace

symbolCode = function (feature_labels, symbol_replace = FALSE) {
  factors = unique(feature_labels)
  symbol_values = c(0:19)
  
  symbol_code = sample(symbol_values, size = length(factors), replace = symbol_replace)
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
