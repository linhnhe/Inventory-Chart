RandomColor <- function(opacity){
  # Create a string that defines a random color
  #
  # Args:
  #   opacity: the degree of transparency of the color
  #
  # Returns:
  #   The string that defines a random color
  
  rand.color <- paste("rgba(", as.character(sample(1:255, 1)), ", ", as.character(sample(1:255, 1)), ", ", 
                      as.character(sample(1:255, 1)), ", ", as.character(opacity), ")", sep = "")
  
  return(rand.color)
}