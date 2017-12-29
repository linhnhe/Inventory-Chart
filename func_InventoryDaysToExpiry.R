InventoryDaysToExpiry <-
function(data, SKUno){
  # Draw a line chart denotes quantity and days to expiry of each batch of an SKU
  #
  # Args:
  #   data: a dataframe with columns: SKU, Date(%Y-%m-%d format), Quantity, Expiration.Date(%Y-%m-%d format)
  #   SKUno: SKU number to be drawn
  #
  # Returns:
  #   The line chart
    
  # Call the package
  require(plotly)
  require(dplyr)
  require(reshape2)
  
  # Make sure that the input dataframe is truely a dataframe
  data <- as.data.frame(data)
    
  # Change the Date & Expiration.Date columns to date format
  data$Date  <- as.Date(data$Date,format = "%Y-%m-%d")
  data$Expiration.Date  <- as.Date(data$Expiration.Date,format = "%Y-%m-%d")
    
  # Calculate days to expiry
  data$Days.To.Expiry <- data$Expiration.Date - data$Date
  
  # Extract the SKU
  draw.data <- subset(data, SKU==SKUno)
  
  # Create the legend title
  legendtitle <- list(yref="paper",xref="paper",y=1.05,x=1.2, text="Expiration date",showarrow=F)
  
  # Get the list of unique batches
  data.unique <- unique(draw.data$Expiration.Date)
  
  # Draw the first batch
  inv.batch1 <- draw.data[draw.data$Expiration.Date == data.unique[1],]
  colnames(inv.batch1)[colnames(inv.batch1) == "Quantity"] <- "value"
  draw.cast1 <- dcast(data = inv.batch1, formula = Days.To.Expiry ~ Expiration.Date)
  draw.cast1 <- draw.cast1[order(draw.cast1$Days.To.Expiry, decreasing = TRUE),]
  p <- plot_ly(draw.cast1, x = draw.cast1[[1]], y = draw.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines') %>%
    layout(xaxis = list(autorange = "reversed", title = "Days to expiry"),
           yaxis = list(title = "Quantity"), annotations=legendtitle)
  
  # Add the remaining batches
  for (i in 2:length(data.unique)){
    inv.batch <- draw.data[draw.data$Expiration.Date == data.unique[i],]
    colnames(inv.batch)[colnames(inv.batch) == "Quantity"] <- "value"
    draw.cast <- dcast(data = inv.batch, formula = Days.To.Expiry ~ Expiration.Date)
    draw.cast <- draw.cast[order(draw.cast$Days.To.Expiry, decreasing = TRUE),]
    
    p <- add_trace(p, x = draw.cast[[1]], y = draw.cast[[2]], name = as.character(data.unique[i]), mode = 'lines')
  }

  # Return the chart
  p

}



