delete.cols <- function(df, col.nums) {
  
  min <- min(col.nums)
  max <- max(col.nums)
  
  # check if column number is invalid
  if(min < 1 | max > length(df)) {
    stop("at least one column number is invalid")
    
  } else if (length(unique(col.nums)) != length(col.nums)) {
    stop("duplicate column numbers are not allowed")
    
  } else {
    
    sorted.nums <- sort(col.nums, decreasing = TRUE)
    for(num in sorted.nums) {
      df <- delete.col(df, num)
    }
  }
  
  # remove used variables
  rm(min)
  rm(max)
  
  # return data frame
  return(df)
}