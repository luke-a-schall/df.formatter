move.cols <- function(df, col.nums, loc.num) {

  # save column names
  headers <- colnames(df)

  #check to make sure proper filling is possible
  if((loc.num + length(col.nums) - 1) > length(df[1,])) {
    stop("invalid data location, results in gap in data")

  } else if(is.element(loc.num, col.nums)) {
    if(loc.num == min(col.nums)) {
      stop("location cannot be relocated to the smallest selected column index")
    }
  }

  # create proper data location
  if(loc.num == 1) {
    temp1 <- data.frame(df[,col.nums])
    tdf <- df[,-col.nums]
    df <- cbind(temp1, tdf)

  } else {
    temp1 <- data.frame(df[,col.nums])
    tdf <- df[,-col.nums]
    temp2 <- data.frame(tdf[,1:(loc.num-1)])
    temp2 <- cbind(temp2, temp1)
    if(length(temp2[1,]) != length(df[1,])) {
      df <- cbind(temp2, tdf[,loc.num:(length(tdf[1,]))])
    } else {
      df <- temp2
    }

    # change column names
    col.names <- headers[col.nums]
    headers <- headers[-col.nums]
    if((length(df) - loc.num + 1) == length(col.nums)) {
      colnames(df) <- c(headers[1:(loc.num-1)], col.names)
    } else {
      colnames(df) <- c(headers[1:(loc.num-1)], col.names, headers[loc.num:length(headers)])
    }
  }

  #clean up variables and return final data frame
  rm(tdf)
  rm(temp1)
  if(loc.num != 1) rm(temp2)
  return(df)
}
