insert.col <- function(df, col.name, col.num, col.data) {

  # save column headers
  headers <- colnames(df)

  # check if column number is valid
  if(col.num < 1 | col.num > (length(df)+1)) {
    stop("column number is invalid")

  } else if (col.num == 1) {
    #create proper headers
    temp1 <- col.name
    headers <- c(temp1, headers)

    #create proper data location
    temp2 <- data.frame(col.data)
    df <- cbind(temp2, df)

  } else if (col.num == (length(df)+1)) {
    # create proper headers
    temp1 <- col.name
    headers <- c(headers, col.name)

    # create proper data location
    temp2 <- data.frame(col.data)
    df <- cbind(df, temp2)

  } else {
    #create proper headers
    temp1 <- headers[1:(col.num-1)]
    temp1 <- c(temp1, col.name)
    headers <- c(temp1, headers[col.num:length(headers)])

    #create proper data location
    temp2 <- data.frame(df[,1:(col.num-1)])
    temp2 <- cbind(temp2, col.data)
    df <- cbind(temp2, df[,col.num:length(df)])
  }

  # change column names
  colnames(df) <- headers

  # delete all used variables
  rm(headers)
  rm(temp1)
  rm(temp2)

  # return data frame
  return(df)
}
