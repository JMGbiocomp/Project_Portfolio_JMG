## === Convert Labels into New Labels === ##
# Function uses pattern recognition to generate a new vector of labels corresponding to a set of feature labels
# Functionality includes capturing a indefinite number of patterns and matching them to a set of new labels of equal length
# arguments:
  # feature_labels = character or numeric vector of feature labels that need to be conveted into a new labels
  # label_pattern = character, numeric or mixed vector of patterns for recognition against the current feature labels
  # label_match = vector of new labels indexed to correspond to the vector used for the label_pattern argument

codeLabel = function(feature_labels, label_pattern, label_match) {
  # error and warning messages
  if (length(label_pattern) != length(label_match)) {stop("provide an equal number of patterns to recognize and their corespodning new labels")}
  
  label_data = c(t(feature_labels))
  new_labels = c()
  for (i in 1:length(label_data)) {
    for (p in 1:length(label_pattern)) {
      if (grepl(pattern = label_pattern[p], x = label_data[i])) {
        new_labels = c(new_labels, label_match[p])
      }
    }
  }
  return(new_labels)
}
