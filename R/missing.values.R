missing.values <- function(df, type = "perc", decimal = 2) {

  # check decimal parameter
  if(type == "sum" && !is.na(decimal)) {
    stop("invalid decimal parameter, must be \"NA\" with \"sum\"")
  } else if(type == "perc" && is.na(decimal)) {
    stop("invalid decimal parameter, must be a positive integer with \"perc\"")
  } else if (type == "perc" && decimal < 0) {
    stop("invalid decimal parameter, must be a positive integer with \"perc\"")
  }

  missingValue <- sapply(colnames(df), function(x) {sum(is.na(df[,x]))})

  if(type == "perc") {
    return(round((missingValue[sapply(missingValue, function(x) {x > 0})]/length(df[,1]))*100, decimal))
  } else if(type == "sum") {
    return(missingValue[sapply(missingValue, function(x) {x > 0})])
  } else {
    stop("invalid type parameter. Specify \"perc\" or \"sum\"")
  }
}
