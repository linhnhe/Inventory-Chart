InventoryDaysToExpirySM <-
  function(data){
    # Draw a small multiple of the inventory days to expiry chart
    #
    # Args:
    #   data: a dataframe with columns: SKU, Date(%Y-%m-%d format), Quantity, Expiration.Date(%Y-%m-%d format)
    #
    # Returns:
    #   The small multiple chart
    
    # Call the package
    require(plotly)
    require(dplyr)
    require(reshape2)
    
    # Make sure that the input dataframe is truely a dataframe
    data <- as.data.frame(data)
    
    # Make sure on date format
    data$Date  <- as.Date(data$Date,format = "%Y-%m-%d")
    data$Expiration.Date  <- as.Date(data$Expiration.Date,format = "%Y-%m-%d")
    
    # Calculate days to expiry
    data$Days.To.Expiry <- data$Expiration.Date - data$Date
    data$Days.To.Expiry <- as.numeric(data$Days.To.Expiry)
    
    # Get the list of all SKUs
    sku.list <- data %>% group_by(SKU) %>% 
      summarise(Max.Inventory = max(Quantity), Max.Days.To.Expiry = max(Days.To.Expiry))
    sku.list <- sku.list %>% arrange(desc(Max.Inventory))
    
    # Find the number of rows the small multiple will have
    # (providing that each row will have at most 6 plots)
    no.rows <- ceiling(nrow(sku.list)/6)
    
    # Arrange each row by descending order of max days to expiry
    for (i in 1:nrow(sku.list)){
      sku.list$Row[i] <- ((i-1) %/% 6) + 1
    }
    sku.list <- sku.list %>% group_by(Row) %>% arrange(desc(Max.Days.To.Expiry), .by_group = TRUE)
    
    ## First SKU
    test <- subset(data, SKU==sku.list[[1]][1])
    test <- test[order(test$Expiration.Date, test$Date, decreasing = F),]
    
    # Get list of all batches of the SKU
    data.unique <- unique(test$Expiration.Date)
    
    # Draw the first batch
    inv.batch1 <- test[test$Expiration.Date == data.unique[1],]
    colnames(inv.batch1)[colnames(inv.batch1) == "Quantity"] <- "value"
    test.cast1 <- dcast(data = inv.batch1, formula = Days.To.Expiry ~ Expiration.Date)
    test.cast1 <- test.cast1[order(test.cast1$Days.To.Expiry, decreasing = TRUE),]
    
    # font style
    f <- list(
      size = 11,
      color = "black")
    
    # annotations
    anno <- list(
      text = as.character(sku.list[[1]][1]),
      font = f,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    
    # Draw the first batch
    p <- plot_ly(test.cast1, x = test.cast1[[1]], y = test.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0.4)', width = 1)) %>%
      layout(annotations = anno, showLegend = FALSE, 
             xaxis = list(autorange = "reversed", title = "Days to expiry"))
    # add_trace(y = test.cast[[8]], name = 'trace 1', mode = 'lines') %>%
    # add_trace(y = test.cast[[9]], name = 'trace 2', mode = 'lines') %>%
    
    # Add the remaining batches
    for (i in 2:length(data.unique)){
      inv.batch <- test[test$Expiration.Date == data.unique[i],]
      colnames(inv.batch)[colnames(inv.batch) == "Quantity"] <- "value"
      test.cast <- dcast(data = inv.batch, formula = Days.To.Expiry ~ Expiration.Date)
      test.cast <- test.cast[order(test.cast$Days.To.Expiry, decreasing = TRUE),]
      
      p <- add_trace(p, x = test.cast[[1]], y = test.cast[[2]], name = as.character(data.unique[i]), mode = 'lines')
    }
    
    p <- hide_legend(p)
    plot.list <- list()
    plot.list[[1]] <- p
    
    ## Next SKUs
    for (i in 2:nrow(sku.list)){
      test <- subset(data, SKU==sku.list[[1]][i])
      test <- test[order(test$Expiration.Date, test$Date, decreasing = F),]
      
      data.unique <- unique(test$Expiration.Date)
      
      # Draw the first batch
      inv.batch1 <- test[test$Expiration.Date == data.unique[1],]
      colnames(inv.batch1)[colnames(inv.batch1) == "Quantity"] <- "value"
      test.cast1 <- dcast(data = inv.batch1, formula = Days.To.Expiry ~ Expiration.Date)
      test.cast1 <- test.cast1[order(test.cast1$Days.To.Expiry, decreasing = TRUE),]
      
      # colnames(test)[colnames(test) == "Quantity"] <- "value"
      # test.cast <- dcast(data = test, formula = Date ~ Expiration.Date)
      # test.cast <- merge(x = test.cast, y = test, by = "Date", all.x = TRUE)
      # test.cast <- subset(test.cast, select = -c(SKU, value, Expiration.Date, Expiration.Text))
      # test.cast <- test.cast[,c(ncol(test.cast), 2:(ncol(test.cast) - 1))]
      
      # annotations
      anno <- list(
        text = as.character(sku.list[[1]][i]),
        font = f,
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      )
      
      p1 <- plot_ly(test.cast1, x = test.cast1[[1]], y = test.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,0.4)', width = 1)) %>%
        layout(annotations = anno, showLegend = FALSE, xaxis = list(autorange = "reversed", title = "Days to expiry"))
      
      # Add the remaining batches
      for (j in 2:length(data.unique)){
        inv.batch <- test[test$Expiration.Date == data.unique[j],]
        colnames(inv.batch)[colnames(inv.batch) == "Quantity"] <- "value"
        test.cast <- dcast(data = inv.batch, formula = Days.To.Expiry ~ Expiration.Date)
        test.cast <- test.cast[order(test.cast$Days.To.Expiry, decreasing = TRUE),]
        
        p1 <- add_trace(p1, x = test.cast[[1]], y = test.cast[[2]], name = as.character(data.unique[i]), mode = 'lines')
      }
      
      p1 <- hide_legend(p1)
      
      plot.list[[i]] <- p1
    }
    
    small.multiple <- subplot(plot.list, nrows = no.rows, shareX = TRUE, shareY = TRUE) 
    # %>% layout(dragmode = "select")
    
    # Return the chart
    return(small.multiple)
    
  }



