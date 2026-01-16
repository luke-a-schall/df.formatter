delete.rows <- function(df, row.nums, rows.numbered = TRUE) {
  
  min <- min(row.nums)
  max <- max(row.nums)
  
  
  # check if column number is invalid
  if(min < 1 | max > length(df[,1])) {
    stop("at least one row number is invalid")
    
  } else if (length(unique(row.nums)) != length(row.nums)) {
    stop("duplicate row numbers are not allowed")
    
  } else {
    
    sorted.nums <- sort(row.nums, decreasing = TRUE)
    for(num in sorted.nums) {
      df <- delete.row(df, num)
    }
  }
  
  if(rows.numbered == TRUE) {
    rownames(df) <- 1:length(df[,1])
  }
  
  # remove used variables
  rm(min)
  rm(max)
  
  # return data frame
  return(df)
}