swap.row <- function(df, row.num1, row.num2) {

  #create proper data location
  temp <- df[row.num1,]
  df[row.num1,] <- df[row.num2,]
  df[row.num2,] <- temp

  #remove variable
  rm(temp)

  # return data frame
  return(df)
}
