#read inventory data of FONO PH
inventory.Ph <- read.csv("merged_inventory_V02.csv")

#Restructure database to move dates to column
inventory.Ph <- reshape2::melt(inventory.Ph,id.vars=c("SKU","Description","Expiration"))

#Rename column Date column and remove X and adjust to date format
inventory.Ph$Date <- sub(".*X","",inventory.Ph$variable)

#Remove null rows
inventory.Ph <- subset(inventory.Ph , value>0)

#Convert Date to date format
inventory.Ph$Date <- gsub("\\.","-",inventory.Ph$Date)
inventory.Ph$Date <- as.Date(inventory.Ph$Date)

#Define if product in stock that are expired
#Expiration to Date
inventory.Ph$Expiration <- as.Date(inventory.Ph$Expiration)

#Define number of days over expiration date
inventory.Ph$Expired <- inventory.Ph$Expiration-inventory.Ph$Date
inventory.Ph$Expired <- as.numeric(inventory.Ph$Expired)


#Check if the date id over the expiration d ste
for(i in 1:nrow(inventory.Ph)){
  if(inventory.Ph$Expired[i] < 0)
    inventory.Ph$Status[i] <- "Expired"
  else inventory.Ph$Status[i] <- "Fresh"
}

#Load second Material Master data
material.mastPhup <- read.csv("#19_20170404 - Item Lsting (Consumer & FoodService).csv",stringsAsFactors = F)
material.mastPhup$Pack.Size <- as.numeric(material.mastPhup$Pack.Size)

#load dplyr
library(dplyr)

#Get total inventory per Day per SKU
inventory.sku <- summarise(group_by(inventory.Ph,SKU,Date,Description,Status,Expiration) ,Total.Inventory=sum(value))
inventory.day <- summarise(group_by(inventory.Ph,Date) ,Total.Inventory=sum(value))

#load stringr
library(stringr)

#load ggplot2
library(ggplot2)


inventory.sku <-merge(inventory.sku,data.frame(material.mastPhup$Branch.Plant,material.mastPhup$Item.Number,material.mastPhup$Pack.Size,material.mastPhup$Pack.Format,material.mastPhup$Stocking.Type), by.x = "SKU",by.y = "material.mastPhup.Item.Number")

#Convert data to numeric type
inventory.sku$material.mastPhup.Pack.Size <- as.numeric(inventory.sku$material.mastPhup.Pack.Size)
inventory.sku$material.mastPhup.Pack.Format <- as.numeric(inventory.sku$material.mastPhup.Pack.Format)

inventory.sku$Weight.per.case <- inventory.sku$material.mastPhup.Pack.Size * inventory.sku$material.mastPhup.Pack.Format

#Load Material Master with product vlue
material.mastPhcost <- read.csv("F17-Q3StandardCost.csv", skip = 4  ,stringsAsFactors = F)

#Add to inventory.sku weight of pack and #packs per case
inventory.sku <-merge(inventory.sku,data.frame(material.mastPhup$Item.Number ,material.mastPhup$Pack.Size ,material.mastPhup$Pack.Format),by.x = "SKU",by.y = "material.mastPhup.Item.Number")

#Add to inventory.sku standard cost in Php/MT
inventory.sku <-merge(inventory.sku,data.frame(material.mastPhcost$Item.No,material.mastPhcost$Total.Cost.in.MT),by.x = "SKU",by.y = "material.mastPhcost.Item.No")

#Get cost per case
inventory.sku$material.mastPhcost.Total.Cost.in.MT <-sub(",","",inventory.sku$material.mastPhcost.Total.Cost.in.MT)
inventory.sku$material.mastPhcost.Total.Cost.in.MT <-as.numeric(inventory.sku$material.mastPhcost.Total.Cost.in.MT)

inventory.sku$Cost.per.case <-inventory.sku$material.mastPhcost.Total.Cost.in.MT/1000*inventory.sku$Weight.per.case

#Total inventory Value
inventory.sku$Total.Inventory.Value <- inventory.sku$Total.Inventory*inventory.sku$Cost.per.case


#load gcookbook
#library(gcookbook)


#Stacked Chart of inventory per Day by SKU
ggplot(inventory.sku,aes(x=Date,y=Total.Inventory, fill=SKU)) +geom_bar(stat="identity")

#Stacked Chart of inventory Value per Day by SKU
ggplot(inventory.sku,aes(x=Date,y=Total.Inventory.Value, fill=SKU)) +geom_bar(stat="identity")


#Stacked Chart of inventory per Day by expirtion
ggplot(inventory.sku,aes(x=Date,y=Total.Inventory, fill=Status)) +geom_bar(stat="identity")

#Get total inventory per Day for 112991
inventory.112991 <- subset(inventory.sku,SKU==112991)
#inventory.112991$Expiration <- toString(inventory.112991$Expiration)

#Stacked Chart of inventory per Day by expiration for 112991
ggplot(inventory.112991,aes(x=Date,y=Total.Inventory, fill=Expiration)) +geom_bar(stat="identity")+ scale_colour_brewer(palette = "Set1")

#Inventory per day by days to expiration for 112991
inventory.sku$Expired <- inventory.sku$Expiration-inventory.sku$Date
inventory.sku$Expired <- as.numeric((inventory.sku$Expired))
inventory.112991 <- subset(inventory.sku,SKU==112991)
ggplot(inventory.112991,aes(x=Date,y=Total.Inventory, fill=Expired)) +geom_bar(stat="identity")+ scale_colour_brewer(palette = "Set1")

#Inventory per day by quantity and days to expiration for 112991
inventory.summary <- summarise(group_by(inventory.sku,SKU,Date,Description,Expiration,Total.Inventory,Expired))
inventory.112991 <- subset(inventory.summary,SKU==112991)
ggplot(inventory.112991,aes(x=Date,y=Expired, fill=Total.Inventory)) +geom_bar(stat="identity")+ scale_colour_brewer(palette = "Set1")

#Inventory movement by expiration dates for 112991
p1 <- ggplot(inventory.112991, aes(x = Date, y = Total.Inventory))
p1 + geom_line(aes(color = Expiration))

#Inventory movement by expiration dates for 112991 (in different colors)
inventory.112991$Expiration.Date <- as.character(inventory.112991$Expiration)
p1 <- ggplot(inventory.112991, aes(x = Date, y = Total.Inventory))
p1 + geom_line(aes(color = Expiration.Date))


#Draw separated chart for each batch
p2 <- p1 + geom_line() + facet_wrap(~Expiration, ncol = 10)

#Creating new theme
(theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = muted("orange"))))

p2 + theme_new

#Change theme
p2 + theme_light()

#Slightly different codes for drawing inventory movement by expiration dates for 112991 in different colors
p3 <- ggplot(inventory.112991, aes(x = Date, y = Total.Inventory, fill = Expiration.Date))
p3 + geom_line(aes(col = Expiration.Date))

#_____________________________________________________________

# VER 2 - 09June2017
setwd("D:/Google Drive/LINH/Fontera Philippines")

library(data.table)
library(dplyr)
library(reshape2)

inventory <- fread("merged_inventory.csv")
inventory <- melt(data = inventory, id.vars = c("SKU", "Description", "Expiration"))
names(inventory)[4] <- "Date"
names(inventory)[5] <- "Quantity"

# Change dates to date format
str(inventory)
inventory$Expiration <- as.Date(x = inventory$Expiration, format = "%Y-%m-%d")
inventory$Date <- as.Date(x = inventory$Date, format = "%Y-%m-%d")
str(inventory)

# Calculate dates to expiry
inventory$Dates.To.Expiry <- inventory$Expiration - inventory$Date
inventory$Dates.To.Expiry <- as.numeric(inventory$Dates.To.Expiry)

# Save full file
write.csv(inventory, file = "Inventory_full.csv")

# Remove 0 quantity
inventory.main <- inventory[inventory$Quantity > 0, ]

# Add Status column
for(i in 1:nrow(inventory.main)){
    if(inventory.main$Dates.To.Expiry[i] < 0)
      inventory.main$Status[i] <- "Expired"
    else inventory.main$Status[i] <- "Fresh"
}

# Save the file
write.csv(inventory.main, file = "Inventory_nonzero.csv")

#____________________________________
 
# ANALYSE INVENTORY OF EACH SKU
inventory <- fread("Inventory_full.csv")

inventory.1064687 <- subset(inventory, SKU == 1064687)

(for(i in 1:nrow(inventory.1064687)){
  if(inventory.1064687$Dates.To.Expiry[i] < 0)
      inventory.1064687$Status[i] <- "Expired"
  else inventory.1064687$Status[i] <- "Fresh"
    })

write.csv(inventory.1064687, file = "Inventory_1064687.csv")

inventory.1064687$Expiration.Date <- as.character(inventory.1064687$Expiration)

ggplot(inventory.1064687, aes(x = Date, y = Quantity, color = Expiration.Date)) + geom_line()

inv.batch1 <- subset(inventory.1064687, Expiration.Date == "2016-09-02")
inv.batch2 <- subset(inventory.1064687, Expiration.Date == "2016-09-17")
inv.batch3 <- subset(inventory.1064687, Expiration.Date == "2017-02-23")
inv.batch4 <- subset(inventory.1064687, Expiration.Date == "2017-04-25")
inv.batch5 <- subset(inventory.1064687, Expiration.Date == "2017-04-30")

rm(i, inv.batch1, inv.batch2, inv.batch3, inv.batch4, inv.batch5, inventory.1064687)
