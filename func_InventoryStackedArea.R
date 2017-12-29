InventoryStackedArea <-
function(data, SKUno){
  # Draw the stacked area chart for quantity by date of each batch (with a unique expiry date)
  # of a specific SKU in inventory.
  #
  # Args:
  #   data: a dataframe with columns: SKU, Date(%Y-%m-%d format), Quantity, Expiration.Date(%Y-%m-%d format)
  #   SKUno: SKU number to be drawn
  #
  # Returns:
  #   The stacked area chart
  
  # Call the package
  require(plotly)
  require(dplyr)
  require(reshape2)
  
  # Call the required function
  source("func_RandomColor.R")
  source("func_RowCumSum.R")


  # Make sure that the Date column is in date format
  data$Date <- as.Date(data$Date,format = "%Y-%m-%d")
  
  # Make sure that the Expiration.Date column is in date format
  data$Expiration.Date <- as.Date(data$Expiration.Date,format = "%Y-%m-%d")
  
  # Sort the data by ascending order of Expiration.Date and Date
  data <- data[order(data$Expiration.Date, data$Date, decreasing = F),]
  
  # Get the sub data set of the SKU number to be drawn
  draw.data <- subset(data, SKU == SKUno)
  
  # Get the list of batches
  data.unique <- unique(draw.data$Expiration.Date)
  
  # Create the dataframe in wide format to be drawn
  draw.data <- subset(draw.data, select = -c(SKU))
  colnames(draw.data)[colnames(draw.data) == "Quantity"] <- "value"
  draw.data.cast <- dcast(data = draw.data, formula = Date ~ Expiration.Date)
  draw.data.sum <- RowCumSum(draw.data.cast)
  
  # Create the legend title
  legendtitle <- list(yref="paper",xref="paper",y=1.05,x=1.2, text="Expiration date",showarrow=F)
  
  # Plot the first batch
  plot <- plot_ly(x = draw.data.sum[[1]], y = draw.data.sum[[ncol(draw.data.sum)]], fill="tozeroy", type = "scatter", mode="none",
                  text=draw.data.cast[[ncol(draw.data.sum)]], hoverinfo='x+text+name', name=as.character(colnames(draw.data.sum)[ncol(draw.data.sum)]), 
                  fillcolor = RandomColor(1))%>% 
    layout(plot, yaxis=list(title="Inventory quantity"), 
           xaxis=list(title="Date"), annotations=legendtitle)
  
  # Add the remaining batches
  for (i in (ncol(draw.data.sum)-1):2){
    plot <- add_trace(plot, y = draw.data.sum[[i]], fill="tozeroy", mode="none",
                   text=draw.data.cast[[i]], hoverinfo='x+text+name', name=as.character(colnames(draw.data.sum)[i]),
                   fillcolor = RandomColor(1))
  }
  
  # Return the plot
  return(plot)
}

