#Market Basket Analysis

#Data from https://archive.ics.uci.edu/ml/datasets/Online%20Retail
#Accessed April 27th, 2018

########################################### Get Data ###########################################################
#setwd("G:/Personal/R coding/Rmarkdown/MarketBasketAnalysis")
setwd("//CARNAGE-DESKTOP/Personal/R coding/Rmarkdown/MarketBasketAnalysis/MarketBasketAnalysis")

#Disable scientific notation
options(scipen = 999)

library(readxl);library(tidyverse);library(arules);library(reshape2);library(arulesViz)

rawData <- read_excel("Online Retail.xlsx")

########################################### Tidy Data ##########################################################

productTrans <- rawData
productTrans$Date <- as.Date(rawData$InvoiceDate)
productTrans$Time <- format(rawData$InvoiceDate, "%H:%M:%S")


########################################### EDA ##########################################################

# # Sales by Time of Day
# salesTime <- productTrans %>%
#     select(Time,InvoiceNo) %>%
#     #group_by(Time) %>%
#     summarise(
#         InvoiceCount = count(InvoiceNo)
#     )
# salesTime

###################################### Association Rules #################################################


#Select only the data we need for the association rules
assocRulesData <- rawData %>%
  select(InvoiceNo,Description)

#Remove returns & move rows that the unit price is 0
noReturnsData <- rawData[rawData$Quantity>0,]
noReturnsData <- noReturnsData[noReturnsData$UnitPrice>0,]


#Transform data into a usable format for the Apriori algorithm
#data_sorted <- noReturnsData[order(noReturnsData$InvoiceNo),]
library(plyr)
#Transform
itemList <- ddply(noReturnsData,c("InvoiceNo","InvoiceDate"),
                  function(df1)paste(df1$Description,
                                     collapse=","))

#Write data to csv because you need to read back in as a transactions object
write.csv(itemList,"market_basket_data.csv", quote = FALSE, row.names = TRUE)

#Create unique list of products for shiny app
productList <- sort(unique(noReturnsData$Description))
write.csv(productList,"product_list.csv",quote = FALSE, row.names = FALSE, col.names = FALSE)

#Read file back in as transactions object
trans <- read.transactions('market_basket_data.csv', format = 'basket', sep=',',quote = "")

#Top 10 most frequent items
top10 <- itemFrequencyPlot(trans,topN = 10,
                           type = 'absolute',
                           xlab = "Item Frequency")

#Generate rules
options(digits = 2)
rules <- apriori(trans,parameter = list(supp = supp, conf = conf))
rules <- sort(rules, by="lift", decreasing = TRUE)

#Prune the duplicate rules out
rules.pruned <- rules[!is.redundant(rules,measure = "confidence")]
rules.pruned <- sort(rules.pruned, by="lift", decreasing = TRUE)

#Inspect first 5 rules
inspect(rules.pruned[1:20])
# 
# #Plot interactive chart
# intPlot <- plot(rules.pruned[1:50], measure=c("support", "lift"), shading = "confidence", interactive = TRUE)
# intPlot

#Rules summary
summary(rules.pruned)

#Plot a subset of rules
subset2 <- head(rules.pruned,50)
plot(subset2)
subset3 <- head(rules.pruned,10)
plot(subset3,method = "graph", control = list(type = "items",main=""))

###########Targeting Specific Products
#Set support and confidence one time
supp <- 0.001
conf <- 0.8

#What are customers likely to buy BEFORE buying "PARTY BUNTING"
bef <- apriori(data = trans,
                     parameter = list(supp=supp, conf = conf),
                     appearance = list(default="lhs",rhs = "PARTY BUNTING"),
                     control = list(verbose = FALSE))
bef <- sort(bef, decreasing = TRUE, by = "confidence")
inspect(bef[1:10])

#What are customers likely to buy AFTER buying "REGENCY CAKESTAND 3 TIER"
aft <- apriori(data = trans,
                     parameter = list(supp=supp, conf = .15),
                     appearance = list(default="rhs",lhs = "CHILLI LIGHTS"),
                     control = list(verbose = FALSE))
aft <- sort(aft, decreasing = TRUE, by = "confidence")
inspect(aft)
