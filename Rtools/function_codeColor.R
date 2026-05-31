## === Create a color scheme for values/features intended for plots === ##
# Function has a predefined color scheme set fitting for colorblindness distinction
# Functionality includes expansion beyond limited colors in cases where sample size is larger than the number of colors as well as handles replacement
# Arguments:
  # feature_data = character or numeric vector for feature labels
  # color_replace = override control for replacement function of sampling

codeColor = function (feature_data, color_replace = FALSE) {
  feature_factors = unique(feature_data)
  distinct_colors = c("black","darkgrey","red","blue","steelblue","orange","violet","lightgreen","magenta","darkred")
  if (length(feature_factors) <= length(distinct_colors)) {
    color_code = sample(x = distinct_colors, size = length(feature_factors), replace = color_replace)
  } else {
    color_choices = colors(distinct = TRUE)
    if (length(feature_factors) > length(color_choices)) {
      color_code = sample(x = color_choices, size = length(feature_factors), replace = TRUE)
    } else {
      color_code = sample(x = color_choices, size = length(feature_factors), replace = color_replace)
    }
  }
  color_match = c()
  
  for (i in 1:length(feature_data)) {
    for (c in 1:length(feature_factors)) {
      if (feature_data[i] == feature_factors[c]) {
        color_match = c(color_match, color_code[c])
      } 
    }
  }
  return(color_match)
}


