separate <- function(df, col.num) {

  # create different filtering factors
  headers <- unique((df)[,col.num])

  y <- 0

  # divide data frame into filtered data frames
  for(x in headers) {
    if(y == 0) {
      temp <- list(subset(df, df[,col.num] == x))
      y <- 1
    } else {
      temp <- c(temp, list(subset(df, df[,col.num] == x)))
    }
  }

  # remove all used variables
  rm(x)
  rm(y)
  rm(headers)

  # return list of data frames
  return(temp)

  # remove used variable
  rm(temp)
}
