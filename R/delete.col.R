delete.col <- function(df, col.num) {
  
  # check if column number is valid
  if(col.num < 1 | col.num > length(df)) {
    stop("column number is invalid")
    
  } else {
    # remove first column
    df <- df[,-(col.num)]
  }
  
  # return data frame
  return(df)
}