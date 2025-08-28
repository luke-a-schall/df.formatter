move.row <- function(df, row.num, loc.num, rows.numbered = TRUE) {

  #create proper data location
  if(loc.num == 1) {
    temp1 <- data.frame(df[row.num,])
    df <- df[-row.num,]
    df <- rbind(temp1, df)

  } else if(loc.num == length(df[,1])) {
    temp1 <- data.frame(df[row.num,])
    df <- df[-row.num,]
    df <- rbind(df, temp1)

  } else {
    temp1 <- data.frame(df[row.num,])
    df <- df[-row.num,]
    temp2 <- data.frame(df[1:(loc.num-1),])
    temp2 <- rbind(temp2, temp1)
    df <- rbind(temp2, df[loc.num:length(df[,1]),])
  }

  # change row names
  if(rows.numbered == TRUE) rownames(df) <- 1:length(df[,1])

  # delete all used variables
  rm(temp1)
  if(loc.num != 1 && loc.num != length(df[,1])) rm(temp2)

  # return data frame
  return(df)
}
