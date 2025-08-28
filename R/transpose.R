transpose <- function(df) {

  # save data frame labels
  col_names <- colnames(df)
  row_names <- rownames(df)

  # make a square data frame
  szdfc <- 0
  szdfr <- 0

  # sets all columns to character type
  for(x in 1:length(df[1,])) {
    df[,x] <- as.character(df[,x])
  }

  # more columns than rows
  if(length(df[1,]) > length(df[,1])) {
    szdfc <- (length(df[1,]) - length(df[,1]))

    for(x in 1:(length(df[1,]) - length(df[,1]))) {
      df <- rbind(df, temp = rep(0, length(df[1,])))
    }
  }

  # more rows than columns
  if(length(df[,1]) > length(df[1,])) {
    szdfr <- (length(df[,1]) - length(df[1,]))

    for(x in 1:(length(df[,1]) - length(df[1,]))) {
      df <- cbind(df, temp = rep(0, length(df[,1])))
    }
  }

  # make list of vectors
  for(x in 1:length(df[1,])) {
    if(x == 1) {
      tempc <- list((df[,x]))
    } else {
      tempc <- c(tempc, list((df[,x])))
    }
  }

  # insert vectors into opposite axis
  for(x in 1:length(df[,1])) {
    df[x,] <- unlist(tempc[x])
  }

  # create original size data frame
  # more columns than rows
  if(szdfc > szdfr) {
    df <- df[,-((length(df[1,]) - szdfc + 1):(length(df[1,])))]
  }

  # more rows than columns
  if(szdfr > szdfc) {
    df <- df[-((length(df[,1]) - szdfr + 1):(length(df[,1]))),]
  }

  # change data frame labels
  rownames(df) <- col_names
  colnames(df) <- row_names

  # delete all used variables
  rm(x)
  rm(tempc)
  rm(szdfc)
  rm(szdfr)
  rm(col_names)
  rm(row_names)

  # return data frame
  return(df)
}
