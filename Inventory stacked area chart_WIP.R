library(plotly)

diamonds1 <- diamonds[which(diamonds$cut == "Fair"),]
density1 <- density(diamonds1$carat)

diamonds2 <- diamonds[which(diamonds$cut == "Ideal"),]
density2 <- density(diamonds2$carat)

p <- plot_ly(x = density1$x, y = density1$y, type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy',
             fillcolor = 'rgba(168, 216, 234, 0.5)',
             line = list(width = 0.5)) %>%
  add_trace(x = ~density2$x, y = ~density2$y, name = 'Ideal cut', fill = 'tozeroy',
            fillcolor = 'rgba(255, 212, 96, 0.5)') %>%
  layout(xaxis = list(title = 'Carat'),
         yaxis = list(title = 'Density'))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="area/colors")
chart_link

#_________________________________________________
inv.batch1 <- inventory.112991[which(inventory.112991$Expiration.Date == as.Date("2017-02-24")),]
inv.batch2 <- inventory.112991[which(inventory.112991$Expiration.Date == as.Date("2017-03-02")),]
inv.batch3 <- inventory.112991[which(inventory.112991$Expiration.Date == as.Date("2017-03-03")),]

p <- plot_ly(x = inv.batch1$Date, y = inv.batch1$Quantity, type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy',
             fillcolor = 'rgba(168, 216, 234, 0.1)',
             line = list(width = 1)) %>%
  add_trace(x = inv.batch2$Date, y = inv.batch2$Quantity, name = 'Ideal cut', fill = 'tozeroy',
            fillcolor = 'rgba(255, 212, 96, 0.1)') %>%
  add_trace(x = inv.batch3$Date, y = inv.batch2$Quantity, name = 'Linh', fill = 'tozeroy',
            fillcolor = 'rgba(176, 23, 31, 0.1)') %>%
  layout(xaxis = list(title = 'Carat'),
         yaxis = list(title = 'Density'))

p

# ________________________________________________

elw <- read.csv("elw.csv")
elw_stack <- read.csv("elw_stack.csv")

plot <- plot_ly(data=elw_stack, x=~year, y=~x10006, fill="tonexty", type = "scatter", mode="lines",
                text=round(elw$x10006, 0), hoverinfo='x+text+name', name="x10006") %>% 
  add_trace(y=~x12018, fill="tonexty", mode="lines",
            text=round(elw$x12018,0), hoverinfo='x+text+name', name="x12018") %>% 
  add_trace(y=~x19000, fill="tonexty", mode="lines",
                    text=round(elw$x19000,0), hoverinfo='x+text+name', name="x19000") %>% 
  add_trace(y=~x20000, fill="tonexty", mode="lines",
            text=round(elw$x20000,0), hoverinfo='x+text+name', name="x20000") %>% 
  add_trace(y=~x99999, fill="tonexty", mode="lines",
            text=round(elw$x99999,0), hoverinfo='x+text+name', name="x99999") %>% 
  layout(plot, yaxis=list(title="Whatever title you wanna use"))

plot
# USING________________________________________________

# 18 Aug 2017

setwd("/run/media/linhnguyen/DATA/Google Drive/R development/Inventory charts")

library(reshape2)
library(readr)

inventory <- read_csv("inventory_data.csv")
inventory <- as.data.frame(inventory)
inventory <- inventory[order(inventory$Date, inventory$Date, decreasing = FALSE),]
test <- subset(inventory, SKU==112991)
test <- subset(test, select = -c(SKU))
colnames(test)[colnames(test) == "Quantity"] <- "value"
test.cast <- dcast(data = test, formula = Date ~ Expiration.Date)
test.cast$Date  <- as.Date(test.cast$Date,format = "%Y-%m-%d")

plot <- plot_ly(x = test.sum[[1]], y = test.sum[[7]], fill="tonexty", type = "scatter", mode="lines",
                text=test.cast[[7]], hoverinfo='x+text+name', name="x10006") %>% 
  add_trace(y = test.sum[[8]], fill="tonexty", mode="lines",
            text=test.cast[[8]], hoverinfo='x+text+name', name="x12018") %>% 
  add_trace(y = test.sum[[9]], fill="tonexty", mode="lines",
            text=test.cast[[9]], hoverinfo='x+text+name', name="x19000") %>% 
  add_trace(y = test.sum[[10]], fill="tonexty", mode="lines",
            text=test.cast[[10]], hoverinfo='x+text+name', name="x20000") %>% 
  add_trace(y = test.sum[[11]], fill="tonexty", mode="lines",
            text=test.cast[[11]], hoverinfo='x+text+name', name="x99999") %>% 
  layout(plot, yaxis=list(title="Whatever title you wanna use"))

plot

# p <- plot_ly(x = test.cast[[1]], y = test.cast[[7]], name = 'Food and Tobacco', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(168, 216, 234, 0.1)') %>%
#   add_trace(y = test.cast[[8]], name = 'Household Operation', fillcolor = 'rgba(255, 212, 96, 0.1)') %>%
#   add_trace(y = test.cast[[9]], name = 'Medical and Health', fillcolor = 'rgba(176, 23, 31, 0.1)') %>%
#   add_trace(y = test.cast[[10]], name = 'Personal Care', fillcolor = 'rgba(17, 231, 131, 0.1)') %>%
#   add_trace(y = test.cast[[11]], name = 'Private Education', fillcolor = 'rgba(16, 39, 31, 0.1)') %>%
#   layout(title = 'United States Personal Expenditures by Categories',
#          xaxis = list(title = "",
#                       showgrid = FALSE),
#          yaxis = list(title = "Expenditures (in billions of dollars)",
#                       showgrid = FALSE))

# 30 Aug 2017 --------


RowCumSum <- function(data, na.rm = FALSE){
  rowlist <- split(data, seq(nrow(data)))
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
  
  return(data.stack)
}

test.sum <- RowCumSum(test.cast)

# for (i in 1:nrow(test.sum)){
#   for (j in 1:ncol(test.sum)){
#     if (is.na(test.sum[i,j])){
#       test.sum[i,j] <- 0
#     }
#   }
# }
# 
# for (i in 1:nrow(test.cast)){
#   for (j in 1:ncol(test.cast)){
#     if (is.na(test.cast[i,j])){
#       test.cast[i,j] <- 0
#     }
#   }
# }


# USING============================================


p <- plot_ly(data = inventory.112991, x = ~Days.To.Expiry, y = ~Quantity, type = 'scatter', mode = 'line', 
             color = ~Expiration.Date, colors = "Set1") %>%
  layout(xaxis = list(autorange = "reversed", title = "Days to expiry"))


# _________________________________________________

RandomColor <- function(opacity){
  rand.color <- paste("rgba(", as.character(sample(1:255, 1)), ", ", as.character(sample(1:255, 1)), ", ", 
                      as.character(sample(1:255, 1)), ", ", as.character(opacity), ")", sep = "")
  rand.color
}

dump("RandomColor", file="myFunction.R")

## Then in a subsequent R session
source("myFunction.R")
#_______________________________________________
inventory <- as.data.frame(inventory)
inventory <- inventory[order(inventory$Date, decreasing = F),]
inventory.112991 <- subset(inventory, SKU==112991)

test <- subset(inventory.112991, (Expiration.Date <= as.Date("2017-03-03") & Expiration.Date >= as.Date("2017-02-24")))

test <- inventory.112991
test.unique <- unique(test$Expiration.Date)

inv.batch1 <- test[which(test$Expiration.Date == test.unique[1]),]

p <- plot_ly(x = inv.batch1$Date, y = inv.batch1$Quantity, type = 'scatter', mode = 'lines', name = as.character(test.unique[1]), fill = 'tozeroy',
             fillcolor = RandomColor(0.2),
             line = list(width = 1)) %>%
  layout(xaxis = list(title = 'Date'),
         yaxis = list(title = 'Quantity'))
for (i in 2:length(test.unique)){
  inv.batch <- test[which(test$Expiration.Date == test.unique[i]),]
  p <- add_trace(p, x = inv.batch$Date, y = inv.batch$Quantity, name = as.character(test.unique[i]), fill = 'tozeroy',
                 fillcolor = RandomColor(0.2))
}

p

# INVENTORY LINE CHART

library(readr)
inventory <- read_csv("inventory_data.csv")
inventory <- as.data.frame(inventory)

inventory$Date  <- as.Date(inventory$Date,format = "%Y-%m-%d")
inventory$Expiration.Date  <- as.Date(inventory$Expiration.Date,format = "%Y-%m-%d")

inventory$Expiration.Text <- as.character(inventory$Expiration.Date)

# Calculate days to expiry
inventory$Days.To.Expiry <- inventory$Expiration.Date - inventory$Date
inventory$Days.To.Expiry <- as.numeric(inventory$Days.To.Expiry)

# Extract the SKU
test <- subset(inventory, SKU==112991)
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

p <- plot_ly(test.cast1, x = test.cast1[[1]], y = test.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(autorange = "reversed", title = "Days to expiry"))
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

# Return the chart
p

# ggplot2____________________________

setwd("/run/media/linhnguyen/DATA/Google Drive/R development/Inventory charts")
library(readr)
inventory <- read_csv("inventory_data.csv")
inventory <- as.data.frame(inventory)

inventory$Date  <- as.Date(inventory$Date,format = "%Y-%m-%d")
inventory$Expiration.Date  <- as.Date(inventory$Expiration.Date,format = "%Y-%m-%d")

inventory$Expiration.Text <- as.character(inventory$Expiration.Date)

# Calculate days to expiry
inventory$Days.To.Expiry <- inventory$Expiration.Date - inventory$Date
inventory$Days.To.Expiry <- as.numeric(inventory$Days.To.Expiry)

# Extract the SKU
test <- subset(inventory, SKU==112991)
test <- test[order(test$Expiration.Date, test$Date, decreasing = F),]

ggplot(data = test, aes(x = Days.To.Expiry, y = Quantity, group = Expiration.Text)) + 
       geom_line(aes(color=Expiration.Text)) + 
       labs(colour = "Expiration date") + xlab("Days to expiry") + scale_x_reverse()


ggplot(data=inventory, aes(x=Days.To.Expiry,y=Quantity,group=factor(Expiration.Text))) +
  geom_line() + scale_x_reverse() + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~SKU, ncol=4) +
  ggtitle("Small Multiples in R") 

# ggsave(filename = "test.png", height = 11, width = 8.5)

# shiny (not using)--------------------------
library(shiny)
library(ggplot2)
library(dplyr)

list.SKUs <- unique(inventory$SKU)
(ui <- fluidPage(
  titlePanel("SKU"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Select variable on x axis:",
                  choices = list.SKUs)),
    mainPanel(
      plotOutput("plotMulti")
    )
  )))

(server <- function(input, output) {
  output$plotMulti <- renderPlot({
    sales <- fread("Test_data_sales_1.csv")
    if (input$x == 'channel.x'){
      if (input$y == 'channel.stk'){
        sales.plot <- sales %>% group_by(Sales.Channel) %>% summarise(Sales = sum(Sales))
        barplot(sales.plot$Sales, names.arg = sales.plot$Sales.Channel)
      }
      else if (input$y == 'group.stk'){
        sales.plot <- sales %>% group_by(Sales.Channel, Product.Group) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Sales.Channel, y = Sales, fill = Product.Group)) + geom_bar(stat = "identity"))
      }
      else if (input$y == 'region.stk'){
        sales.plot <- sales %>% group_by(Sales.Channel, Region) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Sales.Channel, y = Sales, fill = Region)) + geom_bar(stat = "identity"))
      }
    }
    else if (input$x == 'group.x'){
      if (input$y == 'group.stk'){
        sales.plot <- sales %>% group_by(Product.Group) %>% summarise(Sales = sum(Sales))
        barplot(sales.plot$Sales, names.arg = sales.plot$Product.Group)
      }
      else if (input$y == 'channel.stk'){
        sales.plot <- sales %>% group_by(Sales.Channel, Product.Group) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Product.Group, y = Sales, fill = Sales.Channel)) + geom_bar(stat = "identity"))
      }
      else if (input$y == 'region.stk'){
        sales.plot <- sales %>% group_by(Product.Group, Region) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Product.Group, y = Sales, fill = Region)) + geom_bar(stat = "identity"))
      }
    }
    else if (input$x == 'region.x'){
      if (input$y == 'region.stk'){
        sales.plot <- sales %>% group_by(Region) %>% summarise(Sales = sum(Sales))
        barplot(sales.plot$Sales, names.arg = sales.plot$Region)
      }
      else if (input$y == 'channel.stk'){
        sales.plot <- sales %>% group_by(Sales.Channel, Region) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Region, y = Sales, fill = Sales.Channel)) + geom_bar(stat = "identity"))
      }
      else if (input$y == 'group.stk'){
        sales.plot <- sales %>% group_by(Product.Group, Region) %>% summarise(Sales = sum(Sales))
        print(ggplot(data = sales.plot, aes(x = Region, y = Sales, fill = Product.Group)) + geom_bar(stat = "identity"))
      }
    }
  })
})

shinyApp(ui = ui, server = server)

# SMALL MULTIPLE USING CROSSTALK________________________________________
devtools::install_github("rstudio/crosstalk")

library(plotly)
# library(crosstalk) 
library(dplyr)
library(reshape2)

# Examples
d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("black")) %>%
  layout(dragmode = "select")

subplot(
  plot_ly(d, y = ~disp, color = I("black")) %>% add_boxplot(name = "overall"),
  scatterplot, shareY = TRUE
) %>% layout(dragmode = "select")
#-----

# FIRST SKU
library(readr)
inventory <- read_csv("inventory_data.csv")
inventory <- as.data.frame(inventory)

inventory$Date  <- as.Date(inventory$Date,format = "%Y-%m-%d")
inventory$Expiration.Date  <- as.Date(inventory$Expiration.Date,format = "%Y-%m-%d")

inventory$Expiration.Text <- as.character(inventory$Expiration.Date)

# Calculate days to expiry
inventory$Days.To.Expiry <- inventory$Expiration.Date - inventory$Date
inventory$Days.To.Expiry <- as.numeric(inventory$Days.To.Expiry)

# Extract the SKU
test <- subset(inventory, SKU==112991)
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

p <- plot_ly(test.cast1, x = test.cast1[[1]], y = test.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(autorange = "reversed", title = "Days to expiry"))
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

# SECOND SKU
# Extract the SKU
test <- subset(inventory, SKU==1063491)
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

p1 <- plot_ly(test.cast1, x = test.cast1[[1]], y = test.cast1[[2]], name = as.character(data.unique[1]), type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(autorange = "reversed", title = "Days to expiry"))
# add_trace(y = test.cast[[8]], name = 'trace 1', mode = 'lines') %>%
# add_trace(y = test.cast[[9]], name = 'trace 2', mode = 'lines') %>%

# Add the remaining batches
for (i in 2:length(data.unique)){
  inv.batch <- test[test$Expiration.Date == data.unique[i],]
  colnames(inv.batch)[colnames(inv.batch) == "Quantity"] <- "value"
  test.cast <- dcast(data = inv.batch, formula = Days.To.Expiry ~ Expiration.Date)
  test.cast <- test.cast[order(test.cast$Days.To.Expiry, decreasing = TRUE),]
  
  p1 <- add_trace(p1, x = test.cast[[1]], y = test.cast[[2]], name = as.character(data.unique[i]), mode = 'lines')
}

subplot(p, p1) %>% layout(dragmode = "select")

# AUTOMATIZE
# Make sure on date format
inventory$Date  <- as.Date(inventory$Date,format = "%Y-%m-%d")
inventory$Expiration.Date  <- as.Date(inventory$Expiration.Date,format = "%Y-%m-%d")

inventory$Expiration.Text <- as.character(inventory$Expiration.Date)

# Calculate days to expiry
inventory$Days.To.Expiry <- inventory$Expiration.Date - inventory$Date
inventory$Days.To.Expiry <- as.numeric(inventory$Days.To.Expiry)

# Get the list of all SKUs
sku.list <- inventory %>% group_by(SKU) %>% 
  summarise(Max.Inventory = max(Quantity), Max.Days.To.Expiry = max(Days.To.Expiry))
sku.list <- sku.list %>% arrange(desc(Max.Inventory))

# Find the number of rows the small multiple will have
# (providing that each row will have at most 6 plots)
no.rows <- ceiling(nrow(sku.list)/6)

for (i in 1:nrow(sku.list)){
  sku.list$Row[i] <- ((i-1) %/% 6) + 1
}

sku.list <- sku.list %>% group_by(Row) %>% arrange(desc(Max.Days.To.Expiry), .by_group = TRUE)

## First SKU
test <- subset(inventory, SKU==sku.list[[1]][1])
test <- test[order(test$Expiration.Date, test$Date, decreasing = F),]

# Get list of all batches of the SKU
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

# font style
f <- list(
  family = "Courier New, monospace",
  size = 9,
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
  test <- subset(inventory, SKU==sku.list[[1]][i])
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
  # add_trace(y = test.cast[[8]], name = 'trace 1', mode = 'lines') %>%
  # add_trace(y = test.cast[[9]], name = 'trace 2', mode = 'lines') %>%
  
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

subplot(plot.list, nrows = no.rows, shareX = TRUE, shareY = TRUE) %>% layout(dragmode = "select")

# INVENTORY WITH ABC CLASSIFICATION---------------

type <- c("A", "B", "C")

# Get the list of all SKUs
sku.list <- inventory %>% group_by(SKU) %>% summarise(Max.Inventory = max(Quantity))
sku.list <- sku.list[order(sku.list$Max.Inventory, decreasing = TRUE),]

sku.list$Type <- "A"

for (i in 1:nrow(sku.list)){
  sku.list$Type[i] <- type[sample(1:3, 1)]
}

inventory.class <- merge(x = inventory, y = sku.list, by.x = "SKU", by.y = "SKU", all.x = TRUE)
inventory.class <- subset(inventory.class, select = -c(Max.Inventory))

write.csv(x = inventory.class, file = "inventory_data_class.csv")

# FILTER USING CROSSTALK-----------------

# Example
shared_mtcars <- SharedData$new(mtcars)
filter <- filter_checkbox("cyl", "Cylinders", shared_mtcars, ~cyl)
bscols(widths = c(3,NA,NA),
       list(
         filter_checkbox("cyl", "Cylinders", shared_mtcars, ~cyl, inline = TRUE),
         filter_slider("hp", "Horsepower", shared_mtcars, ~hp, width = "100%"),
         filter_select("auto", "Automatic", shared_mtcars, ~ifelse(am == 0, "Yes", "No"))
       ),
       d3scatter(shared_mtcars, ~wt, ~mpg, ~factor(cyl), width="100%", height=250),
       d3scatter(shared_mtcars, ~hp, ~qsec, ~factor(cyl), width="100%", height=250)
)

# Create filter
shared_inventory <- SharedData$new(inventory.class)
filter <- filter_checkbox("x", "Inventory classification:", shared_inventory, ~ Type)

plot <- InventoryDaysToExpirySM1(inventory.class, x)
bscols(filter, plot)

