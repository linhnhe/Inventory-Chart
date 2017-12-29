RowCumSum <- function(data){
  # Calculate the row cummulative sum of each cell in a dataframe
  # and replace the value of the cell with the calculated cummulative sum
  #
  # Args:
  #   data: a dataframe in wide format (data in numeric data format)
  #
  # Returns:
  #   The new dataframe
  
  # Make sure that the input dataframe is really a dataframe
  data <- as.data.frame(data)
  
  # Split all rows of the dataframe into lists
  rowlist <- split(data, seq(nrow(data)))
  
  # Create the output dataframe
  data.stack <- data
  for (i in 1:nrow(data)){
    x <- rowlist[[i]]
    y <- unname(unlist(x[1,2:ncol(x)]))
    for (j in 2:ncol(data)){
      if (!is.na(data[i,j])){
        data.stack[i,j] <- sum(y[1:(j-1)], na.rm = TRUE)
      }
    }
  }
  
  # Return the output dataframe
  return(data.stack)
}