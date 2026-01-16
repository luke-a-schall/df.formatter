delete.row <- function(df, row.num, rows.numbered = TRUE) {
  
  # check if column number is valid
  if(row.num < 1 | row.num > length(df[,1])) {
    stop("row number is invalid")
    
  } else {
    # remove specified row
    df <- df[-(row.num),]
  }
  
  if(rows.numbered == TRUE) {
    rownames(df) <- 1:length(df[,1])
  }

  # return data frame
  return(df)
}