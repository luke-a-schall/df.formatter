insert.row <- function(df, row.num, row.data, rows.numbered = TRUE, row.name = NULL) {

  # save column headers
  headers <- colnames(df)

  if(row.num < 1 | row.num > (length(df[,1])+1)) {
    stop("row number is invalid")

  } else if (row.num == 1) {
    #create proper data location
    temp <- data.frame(row.data)
    colnames(temp) <- colnames(df)
    df <- rbind(temp, df)

  } else if (row.num == (length(df[,1])+1)) {
    # create proper data location
    temp <- data.frame(row.data)
    colnames(temp) <- colnames(df)
    df <- rbind(df, temp)

  } else {
    #create proper data location
    temp <- data.frame(df[1:(row.num-1),])
    temp <- rbind(temp, row.data)
    df <- data.frame(df[row.num:length(df[,1]),])
    colnames(temp) <- colnames(df)
    df <- rbind(temp, df)
  }

  if(rows.numbered == TRUE) {
    rownames(df) <- 1:length(df[,1])
  } else if (is.null(row.name)) {
    stop("if \"rows.numbered = FALSE\", \"row.name\" cannot be NULL")
  } else {
    rownames(df)[row.num] <- row.name
  }

  # change column names
  colnames(df) <- headers

  # delete used variable
  rm(temp)
  rm(headers)

  # return data frame
  return(df)
}
