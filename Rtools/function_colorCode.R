## === Create a color scheme for values/features intended for plots === ##
#
#
# Arguments:
  # feature_data
  # color_replace

colorCode = function (feature_data, color_replace = FALSE) {
  factors = unique(feature_data)
  color_code = sample(x = colors(distinct = TRUE), size = length(factors), replace = color_replace)
  color_match = c()
  
  for (i in 1:length(feature_data)) {
    for (c in 1:length(color_code)) {
      if (feature_data[i] == factors[c]) {
        color_match = c(color_match, color_code[c])
      } 
    }
  }
  return(color_match)
}


