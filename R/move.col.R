move.col <- function(df, col.num, loc.num) {

  # save column names
  headers <- colnames(df)

  # check if column number is valid
  if(col.num < 1 || col.num > length(df)) {
    stop("column number is invalid")

  } else if (loc.num < 1 || loc.num > length(df)) {
    stop("location number is invalid")

  } else if(loc.num == 1) {
    temp1 <- data.frame(df[,col.num])
    df <- df[,-col.num]
    df <- cbind(temp1, df)
    colnames(df) <- c(headers[col.num], headers[-col.num])

  } else if(loc.num == length(df)) {
    temp1 <- data.frame(df[,col.num])
    df <- df[,-col.num]
    df <- cbind(df, temp1)
    colnames(df) <- c(headers[-col.num], headers[col.num])

  } else {
    temp1 <- data.frame(df[,col.num])
    df <- df[,-col.num]
    temp2 <- data.frame(df[,1:(loc.num-1)])
    temp2 <- cbind(temp2, temp1)
    df <- cbind(temp2, df[,loc.num:length(df[1,])])

    col.name <- headers[col.num]
    headers <- headers[-col.num]
    colnames(df) <- c(headers[1:(loc.num-1)], col.name, headers[loc.num:length(headers)])
  }

  #clean up variables and return final data frame
  rm(temp1)
  rm(headers)
  if(loc.num != 1 && loc.num != length(df)) rm(temp2,col.name)

  return(df)
}
