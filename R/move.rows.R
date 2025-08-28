move.rows <- function(df, row.nums, loc.num, rows.numbered = TRUE) {

  #check to make sure proper filling is possible
  if((loc.num + length(row.nums) - 1) > length(df[,1])) {
    stop("invalid data location")

  } else if(is.element(loc.num, row.nums)) {
    if(loc.num == min(row.nums)) {
      stop("location cannot be relocated to the smallest selected row index")
    }
  }

  #create proper data location
  if(loc.num == 1) {
    temp1 <- data.frame(df[row.nums,])
    tdf <- df[-row.nums,]
    df <- rbind(temp1, tdf)

  } else {
    temp1 <- data.frame(df[row.nums,])
    tdf <- df[-row.nums,]
    temp2 <- data.frame(tdf[1:(loc.num-1),])
    temp2 <- rbind(temp2, temp1)
    if(length(temp2[,1]) != length(df[,1])) {
      df <- rbind(temp2, tdf[loc.num:(length(tdf[,1])),])
    } else {
      df <- temp2
    }
  }

  if(rows.numbered == TRUE) rownames(df) <- 1:length(df[,1])

  #clean up variables and return final data frame
  rm(tdf)
  rm(temp1)
  if(loc.num != 1) rm(temp2)
  return(df)
}
