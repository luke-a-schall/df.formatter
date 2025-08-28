swap.col <- function(df, col.num1, col.num2) {

  #create proper headers
  headers <- colnames(df)[col.num1]
  colnames(df)[col.num1] <- colnames(df)[col.num2]
  colnames(df)[col.num2] <- headers

  #create proper data location
  temp1 <- df[,col.num1]
  df[,col.num1] <- df[,col.num2]
  df[,col.num2] <- temp1

  # remove all used variables
  rm(headers)
  rm(temp1)

  # return data frame
  return(df)
}
