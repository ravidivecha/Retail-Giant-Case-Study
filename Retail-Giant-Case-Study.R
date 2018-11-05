setwd("C:\\Users\\eravdiv\\OneDrive - Ericsson AB\\Ravi\\PG-Data-Science\\Retail-Giant-Sales-Case-Study")

# Business understanding:
#   
# "Global Mart" is an online store super giant having worldwide operations. It takes orders and 
# delivers across the globe and deals with all the major product categories - consumer, corporate & home office.
# 
# Now as a sales/operations manager, you want to finalise the plan for the next 6 months.  
# So, you want to forecast the sales and the demand for the next 6 months, that would help you manage 
# the revenue and inventory accordingly.
# 
# The store caters to 7 different market segments and in 3 major categories. You want to forecast at 
# this granular level, so you subset your data into 21 (7*3) buckets before analysing these data.
# 
# But not all of these 21 market buckets are important from the store's point of view. So you need to find 
# out 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments.


# Data Understanding:
#   
# The data currently has the transaction level data, where each row represents a particular 
# order made on the online store. There are 24 attributes related to each such transaction. 
# The "Market" attribute has 7-factor levels representing the geographical market sector that the 
# customer belongs to. The "Segment" attribute tells which of the 3 segments that customer belongs to.


# Data preparation:
# 
# You would need to first segment the whole dataset into the 21 subsets based on the market and the 
# customer segment level. Next, comes the most important data preparation step. That is to convert the 
# transaction-level data into a time series. Thus, you would need to aggregate 
# the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive at monthly 
# values for these attributes. Once, you arrive at these 3 time series for each of the 21 segments, 
# we need to find the 2 most profitable and consistently profitable segments. For this, the metric 
# that you can use is the coefficient of variation of the Profit for all 21 market segments.

# Model building:
#   
# Once you arrive at the 2 most profitable segments, the next challenge is to forecast the 
# sales and quantity for the next 6 months. You are supposed to use classical decomposition and 
# auto ARIMA for forecasting. Also, it is advised that you smoothen the data before you perform 
# classical decomposition.
# 
# Model evaluation:
#   
# Once you come up with a satisfactory model, the next step would be to forecast the sales/demand 
# for next 6 months using this model. To test the accuracy of your forecast, you must initially 
# separate out the last 6 months values from your dataset, after aggregating the transaction level 
# data into the monthly data. Then check your 6 months forecast using the out-of-sample figures. You 
# can use MAPE for this.

library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(ggplot2)

# Approach followed - 
# 1. Import files and perform data cleaning and missing value inclusion
# 2. Segment the whole dataset into the 21 subsets based on the market and the customer segment level.
# 3. Aggregate the data into 21 market segments
# 4. Find the 2 most profitable and consistently profitable segments
# 5. Forecast the sales and quantity for the next 6 months
# 5.1 smoothen the data
# 5.2 classical decomposition
# 5.3 auto ARIMA
# 6. Forecast the sales/demand for next 6 months using this model


box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

# box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
#                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
#                     legend.position="none")

# 1. Import files and perform data cleaning and missing value inclusion

Global_Superstore <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)
View (Global_Superstore)
summary(Global_Superstore)
str(Global_Superstore)
nrow(Global_Superstore)
ncol(Global_Superstore)

# Missing value operations.
colSums(is.na(Global_Superstore))
print(paste("There are blank values for postal code"))

## Cleaning for each variable

# Row.ID
table(duplicated(Global_Superstore$Row.ID))
print(paste("There are no duplicates in Row.ID"))

# Order.ID
table(factor(Global_Superstore$Order.ID))

# Order.Date
Global_Superstore$Order.Date <- as.Date(Global_Superstore$Order.Date,"%d-%m-%Y")
Global_Superstore$Order.Date <- format(Global_Superstore$Order.Date,"%Y %m")
typeof(Global_Superstore$Order.Date)

# Ship.Date
Global_Superstore$Ship.Date <- as.Date(Global_Superstore$Ship.Date,"%d-%m-%Y")
Global_Superstore$Ship.Date <- format(Global_Superstore$Ship.Date,"%Y %m")
typeof(Global_Superstore$Ship.Date)

# Ship.Mode
summary(Global_Superstore$Ship.Mode)
factor(Global_Superstore$Ship.Mode)
table(Global_Superstore$Ship.Mode)

# Customer.ID 
factor(Global_Superstore$Customer.ID)
table(Global_Superstore$Customer.ID)

# Customer.Name
str(Global_Superstore$Customer.Name)

# Segment
factor(Global_Superstore$Segment)
table(Global_Superstore$Segment)

# City
factor(Global_Superstore$City)
table(Global_Superstore$City)

# State
factor(Global_Superstore$State)
table(Global_Superstore$State)

# Country
factor(Global_Superstore$Country)
table(Global_Superstore$Country)

# Postal.Code
factor(Global_Superstore$Postal.Code)
length(Global_Superstore$Postal.Code[which(Global_Superstore$Country != "United States")])
table(is.na(Global_Superstore$Postal.Code))
print(paste("Postal.Code for country United States is only listed in the table"))

# Market
factor(Global_Superstore$Market)
table(Global_Superstore$Market)

# Region
factor(Global_Superstore$Region)
table(Global_Superstore$Region)

# Product.ID
str(Global_Superstore$Product.ID)
table(Global_Superstore$Product.ID)

# Category
factor(Global_Superstore$Category)
table(Global_Superstore$Category)

# Sub.Category
factor(Global_Superstore$Sub.Category)
table(Global_Superstore$Sub.Category)

# Product.Name  
factor(Global_Superstore$Product.Name)
table(Global_Superstore$Product.Name)

# Sales
summary(Global_Superstore$Sales)
str(Global_Superstore$Sales)

# Quantity
summary(Global_Superstore$Quantity)
str(Global_Superstore$Quantity)

# Discount
summary(Global_Superstore$Discount)
str(Global_Superstore$Discount)

# Profit
summary(Global_Superstore$Profit)
str(Global_Superstore$Profit)

# Shipping.Cost
summary(Global_Superstore$Shipping.Cost)
str(Global_Superstore$Shipping.Cost)

# Order.Priority
factor(Global_Superstore$Order.Priority)
table(Global_Superstore$Order.Priority)

# 2. Segment the whole dataset into the 21 subsets based on the market and the customer segment level.

US_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "US", Global_Superstore$Segment == "Consumer")
US_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "US", Global_Superstore$Segment == "Corporate")
US_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "US", Global_Superstore$Segment == "Home Office")
length(unique(US_Consumer$Order.Date))
length(unique(US_Corporate$Order.Date))
length(unique(US_Home_Office$Order.Date))

APAC_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "APAC", Global_Superstore$Segment == "Consumer")
APAC_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "APAC", Global_Superstore$Segment == "Corporate")
APAC_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "APAC", Global_Superstore$Segment == "Home Office")
length(unique(APAC_Consumer$Order.Date))
length(unique(APAC_Corporate$Order.Date))
length(unique(APAC_Home_Office$Order.Date))

EU_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "EU", Global_Superstore$Segment == "Consumer")
EU_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "EU", Global_Superstore$Segment == "Corporate")
EU_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "EU", Global_Superstore$Segment == "Home Office")
length(unique(EU_Consumer$Order.Date))
length(unique(EU_Corporate$Order.Date))
length(unique(EU_Home_Office$Order.Date))

Africa_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "Africa", Global_Superstore$Segment == "Consumer")
Africa_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "Africa", Global_Superstore$Segment == "Corporate")
Africa_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "Africa", Global_Superstore$Segment == "Home Office")
length(unique(Africa_Consumer$Order.Date))
length(unique(Africa_Corporate$Order.Date))
length(unique(Africa_Home_Office$Order.Date))

EMEA_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "EMEA", Global_Superstore$Segment == "Consumer")
EMEA_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "EMEA", Global_Superstore$Segment == "Corporate")
EMEA_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "EMEA", Global_Superstore$Segment == "Home Office")
length(unique(EMEA_Consumer$Order.Date))
length(unique(EMEA_Corporate$Order.Date))
length(unique(EMEA_Home_Office$Order.Date))

LATAM_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "LATAM", Global_Superstore$Segment == "Consumer")
LATAM_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "LATAM", Global_Superstore$Segment == "Corporate")
LATAM_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "LATAM", Global_Superstore$Segment == "Home Office")
length(unique(LATAM_Consumer$Order.Date))
length(unique(LATAM_Corporate$Order.Date))
length(unique(LATAM_Home_Office$Order.Date))

Canada_Consumer <- filter(Global_Superstore, Global_Superstore$Market == "Canada", Global_Superstore$Segment == "Consumer")
Canada_Corporate <- filter(Global_Superstore, Global_Superstore$Market == "Canada", Global_Superstore$Segment == "Corporate")
Canada_Home_Office <- filter(Global_Superstore, Global_Superstore$Market == "Canada", Global_Superstore$Segment == "Home Office")
length(unique(Canada_Consumer$Order.Date))
sort(unique(Canada_Consumer$Order.Date))
length(unique(Canada_Corporate$Order.Date))
length(unique(Canada_Home_Office$Order.Date))

# 3. Aggregate the data into 21 market segments

# Thus, you would need to aggregate 
# the 3 attributes  - Sales, Quantity & Profit, over the Order Date to arrive at monthly 
# values for these attributes. Once, you arrive at these 3 time series for each of the 21 segments, 
# we need to find the 2 most profitable and consistently profitable segments. For this, the metric 
# that you can use is the coefficient of variation of the Profit for all 21 market segments

# US_Consumer
TimeSeries_US_Train <- aggregate(US_Consumer$Sales, by = list(US_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_US_Train) <- c("Month", "Consumer_Sales")
TimeSeries_US_Train$Tech_Qty <- aggregate(US_Consumer$Quantity, by = list(US_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Tech_Profit <- aggregate(US_Consumer$Profit, by = list(US_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Month <- 1:nrow(TimeSeries_US_Train)

US_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_US_Train$Tech_Profit)/mean(TimeSeries_US_Train$Tech_Profit) )
print(paste("US_Consumer_Profit coefficient of variation of the Profit is :", US_Consumer_Profit_Train_covar, "%"))

COV_DF <- data.frame(Names = "US_Consumer_Profit", COV = US_Consumer_Profit_Train_covar)
View(COV_DF)

# US_Corporate

TimeSeries_US_Train$Corporate_Sales <- aggregate(US_Corporate$Sales, by = list(US_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Corporate_Qty <- aggregate(US_Corporate$Quantity, by = list(US_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Corporate_Profit <- aggregate(US_Corporate$Profit, by = list(US_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_US_Train)

US_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_US_Train$Corporate_Profit)/mean(TimeSeries_US_Train$Corporate_Profit) )
print(paste("US_Corporate_Profit coefficient of variation of the Profit is :", US_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="US_Corporate_Profit", COV = US_Corporate_Profit_Train_covar))
View(COV_DF)

# US_Home_Office

TimeSeries_US_Train$Home_Office_Sales <- aggregate(US_Home_Office$Sales, by = list(US_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Home_Office_Qty <- aggregate(US_Home_Office$Quantity, by = list(US_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_US_Train$Home_Office_Profit <- aggregate(US_Home_Office$Profit, by = list(US_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_US_Train)

US_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_US_Train$Home_Office_Profit)/mean(TimeSeries_US_Train$Home_Office_Profit) )
print(paste("US_Home_Office_Profit coefficient of variation of the Profit is :", US_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="US_Home_Office_Profit", COV = US_Home_Office_Profit_Train_covar))
View(COV_DF)

# APAC_Consumer
TimeSeries_APAC_Train <- aggregate(APAC_Consumer$Sales, by = list(APAC_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_APAC_Train) <- c("Month", "Consumer_Sales")

TimeSeries_APAC_Train$Tech_Qty <- aggregate(APAC_Consumer$Quantity, by = list(APAC_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_APAC_Train$Tech_Profit <- aggregate(APAC_Consumer$Profit, by = list(APAC_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_APAC_Train$Month <- 1:nrow(TimeSeries_APAC_Train)
View(TimeSeries_APAC_Train)

APAC_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_APAC_Train$Tech_Profit)/mean(TimeSeries_APAC_Train$Tech_Profit) )
print(paste("APAC_Consumer_Profit coefficient of variation of the Profit is :", APAC_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "APAC_Consumer_Profit", COV = APAC_Consumer_Profit_Train_covar))
View(COV_DF)

# APAC_Corporate

TimeSeries_APAC_Train$Corporate_Sales <- aggregate(APAC_Corporate$Sales, by = list(APAC_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_APAC_Train$Corporate_Qty <- aggregate(APAC_Corporate$Quantity, by = list(APAC_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_APAC_Train$Corporate_Profit <- aggregate(APAC_Corporate$Profit, by = list(APAC_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_APAC_Train)

APAC_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_APAC_Train$Corporate_Profit)/mean(TimeSeries_APAC_Train$Corporate_Profit) )
print(paste("APAC_Corporate_Profit coefficient of variation of the Profit is :", APAC_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="APAC_Corporate_Profit", COV = APAC_Corporate_Profit_Train_covar))
View(COV_DF)

# APAC_Home_Office

TimeSeries_APAC_Train$Home_Office_Sales <- aggregate(APAC_Home_Office$Sales, by = list(APAC_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_APAC_Train$Home_Office_Qty <- aggregate(APAC_Home_Office$Quantity, by = list(APAC_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_APAC_Train$Home_Office_Profit <- aggregate(APAC_Home_Office$Profit, by = list(APAC_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_APAC_Train)

APAC_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_APAC_Train$Home_Office_Profit)/mean(TimeSeries_APAC_Train$Home_Office_Profit) )
print(paste("APAC_Home_Office_Profit coefficient of variation of the Profit is :", APAC_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="APAC_Home_Office_Profit", COV = APAC_Home_Office_Profit_Train_covar))
View(COV_DF)

# EU_Consumer
TimeSeries_EU_Train <- aggregate(EU_Consumer$Sales, by = list(EU_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_EU_Train) <- c("Month", "Consumer_Sales")

TimeSeries_EU_Train$Tech_Qty <- aggregate(EU_Consumer$Quantity, by = list(EU_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EU_Train$Tech_Profit <- aggregate(EU_Consumer$Profit, by = list(EU_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_EU_Train$Month <- 1:nrow(TimeSeries_EU_Train)
View(TimeSeries_EU_Train)

EU_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_EU_Train$Tech_Profit)/mean(TimeSeries_EU_Train$Tech_Profit) )
print(paste("EU_Consumer_Profit coefficient of variation of the Profit is :", EU_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "EU_Consumer_Profit", COV = EU_Consumer_Profit_Train_covar))
View(COV_DF)

# EU_Corporate

TimeSeries_EU_Train$Corporate_Sales <- aggregate(EU_Corporate$Sales, by = list(EU_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EU_Train$Corporate_Qty <- aggregate(EU_Corporate$Quantity, by = list(EU_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EU_Train$Corporate_Profit <- aggregate(EU_Corporate$Profit, by = list(EU_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_EU_Train)

EU_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_EU_Train$Corporate_Profit)/mean(TimeSeries_EU_Train$Corporate_Profit) )
print(paste("EU_Corporate_Profit coefficient of variation of the Profit is :", EU_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="EU_Corporate_Profit", COV = EU_Corporate_Profit_Train_covar))
View(COV_DF)

# EU_Home_Office

TimeSeries_EU_Train$Home_Office_Sales <- aggregate(EU_Home_Office$Sales, by = list(EU_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EU_Train$Home_Office_Qty <- aggregate(EU_Home_Office$Quantity, by = list(EU_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EU_Train$Home_Office_Profit <- aggregate(EU_Home_Office$Profit, by = list(EU_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_EU_Train)

EU_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_EU_Train$Home_Office_Profit)/mean(TimeSeries_EU_Train$Home_Office_Profit) )
print(paste("EU_Home_Office_Profit coefficient of variation of the Profit is :", EU_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="EU_Home_Office_Profit", COV = EU_Home_Office_Profit_Train_covar))
View(COV_DF)



# Africa_Consumer
TimeSeries_Africa_Train <- aggregate(Africa_Consumer$Sales, by = list(Africa_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_Africa_Train) <- c("Month", "Consumer_Sales")

TimeSeries_Africa_Train$Tech_Qty <- aggregate(Africa_Consumer$Quantity, by = list(Africa_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Africa_Train$Tech_Profit <- aggregate(Africa_Consumer$Profit, by = list(Africa_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_Africa_Train$Month <- 1:nrow(TimeSeries_Africa_Train)
# View(TimeSeries_Africa_Train)

Africa_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_Africa_Train$Tech_Profit)/mean(TimeSeries_Africa_Train$Tech_Profit) )
print(paste("Africa_Consumer_Profit coefficient of variation of the Profit is :", Africa_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "Africa_Consumer_Profit", COV = Africa_Consumer_Profit_Train_covar))
View(COV_DF)

# Africa_Corporate

TimeSeries_Africa_Train$Corporate_Sales <- aggregate(Africa_Corporate$Sales, by = list(Africa_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Africa_Train$Corporate_Qty <- aggregate(Africa_Corporate$Quantity, by = list(Africa_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Africa_Train$Corporate_Profit <- aggregate(Africa_Corporate$Profit, by = list(Africa_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_Africa_Train)

Africa_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_Africa_Train$Corporate_Profit)/mean(TimeSeries_Africa_Train$Corporate_Profit) )
print(paste("Africa_Corporate_Profit coefficient of variation of the Profit is :", Africa_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="Africa_Corporate_Profit", COV = Africa_Corporate_Profit_Train_covar))
View(COV_DF)

# Africa_Home_Office

TimeSeries_Africa_Train$Home_Office_Sales <- aggregate(Africa_Home_Office$Sales, by = list(Africa_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Africa_Train$Home_Office_Qty <- aggregate(Africa_Home_Office$Quantity, by = list(Africa_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Africa_Train$Home_Office_Profit <- aggregate(Africa_Home_Office$Profit, by = list(Africa_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_Africa_Train)

Africa_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_Africa_Train$Home_Office_Profit)/mean(TimeSeries_Africa_Train$Home_Office_Profit) )
print(paste("Africa_Home_Office_Profit coefficient of variation of the Profit is :", Africa_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="Africa_Home_Office_Profit", COV = Africa_Home_Office_Profit_Train_covar))
View(COV_DF)

# EMEA_Consumer
TimeSeries_EMEA_Train <- aggregate(EMEA_Consumer$Sales, by = list(EMEA_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_EMEA_Train) <- c("Month", "Consumer_Sales")

TimeSeries_EMEA_Train$Tech_Qty <- aggregate(EMEA_Consumer$Quantity, by = list(EMEA_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EMEA_Train$Tech_Profit <- aggregate(EMEA_Consumer$Profit, by = list(EMEA_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_EMEA_Train$Month <- 1:nrow(TimeSeries_EMEA_Train)
# View(TimeSeries_EMEA_Train)

EMEA_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_EMEA_Train$Tech_Profit)/mean(TimeSeries_EMEA_Train$Tech_Profit) )
print(paste("EMEA_Consumer_Profit coefficient of variation of the Profit is :", EMEA_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "EMEA_Consumer_Profit", COV = EMEA_Consumer_Profit_Train_covar))
View(COV_DF)

# EMEA_Corporate

TimeSeries_EMEA_Train$Corporate_Sales <- aggregate(EMEA_Corporate$Sales, by = list(EMEA_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EMEA_Train$Corporate_Qty <- aggregate(EMEA_Corporate$Quantity, by = list(EMEA_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EMEA_Train$Corporate_Profit <- aggregate(EMEA_Corporate$Profit, by = list(EMEA_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_EMEA_Train)

EMEA_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_EMEA_Train$Corporate_Profit)/mean(TimeSeries_EMEA_Train$Corporate_Profit) )
print(paste("EMEA_Corporate_Profit coefficient of variation of the Profit is :", EMEA_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="EMEA_Corporate_Profit", COV = EMEA_Corporate_Profit_Train_covar))
View(COV_DF)

# EMEA_Home_Office

TimeSeries_EMEA_Train$Home_Office_Sales <- aggregate(EMEA_Home_Office$Sales, by = list(EMEA_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EMEA_Train$Home_Office_Qty <- aggregate(EMEA_Home_Office$Quantity, by = list(EMEA_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_EMEA_Train$Home_Office_Profit <- aggregate(EMEA_Home_Office$Profit, by = list(EMEA_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_EMEA_Train)

EMEA_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_EMEA_Train$Home_Office_Profit)/mean(TimeSeries_EMEA_Train$Home_Office_Profit) )
print(paste("EMEA_Home_Office_Profit coefficient of variation of the Profit is :", EMEA_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="EMEA_Home_Office_Profit", COV = EMEA_Home_Office_Profit_Train_covar))
View(COV_DF)

# LATAM_Consumer
TimeSeries_LATAM_Train <- aggregate(LATAM_Consumer$Sales, by = list(LATAM_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_LATAM_Train) <- c("Month", "Consumer_Sales")

TimeSeries_LATAM_Train$Tech_Qty <- aggregate(LATAM_Consumer$Quantity, by = list(LATAM_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_LATAM_Train$Tech_Profit <- aggregate(LATAM_Consumer$Profit, by = list(LATAM_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_LATAM_Train$Month <- 1:nrow(TimeSeries_LATAM_Train)
# View(TimeSeries_LATAM_Train)

LATAM_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_LATAM_Train$Tech_Profit)/mean(TimeSeries_LATAM_Train$Tech_Profit) )
print(paste("LATAM_Consumer_Profit coefficient of variation of the Profit is :", LATAM_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "LATAM_Consumer_Profit", COV = LATAM_Consumer_Profit_Train_covar))
View(COV_DF)

# LATAM_Corporate

TimeSeries_LATAM_Train$Corporate_Sales <- aggregate(LATAM_Corporate$Sales, by = list(LATAM_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_LATAM_Train$Corporate_Qty <- aggregate(LATAM_Corporate$Quantity, by = list(LATAM_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_LATAM_Train$Corporate_Profit <- aggregate(LATAM_Corporate$Profit, by = list(LATAM_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_LATAM_Train)

LATAM_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_LATAM_Train$Corporate_Profit)/mean(TimeSeries_LATAM_Train$Corporate_Profit) )
print(paste("LATAM_Corporate_Profit coefficient of variation of the Profit is :", LATAM_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="LATAM_Corporate_Profit", COV = LATAM_Corporate_Profit_Train_covar))
View(COV_DF)

# LATAM_Home_Office

TimeSeries_LATAM_Train$Home_Office_Sales <- aggregate(LATAM_Home_Office$Sales, by = list(LATAM_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_LATAM_Train$Home_Office_Qty <- aggregate(LATAM_Home_Office$Quantity, by = list(LATAM_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_LATAM_Train$Home_Office_Profit <- aggregate(LATAM_Home_Office$Profit, by = list(LATAM_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_LATAM_Train)

LATAM_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_LATAM_Train$Home_Office_Profit)/mean(TimeSeries_LATAM_Train$Home_Office_Profit) )
print(paste("LATAM_Home_Office_Profit coefficient of variation of the Profit is :", LATAM_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="LATAM_Home_Office_Profit", COV = LATAM_Home_Office_Profit_Train_covar))
View(COV_DF)

# Canada_Consumer
TimeSeries_Canada_Train <- aggregate(Canada_Consumer$Sales, by = list(Canada_Consumer$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_Canada_Train) <- c("Month", "Consumer_Sales")

TimeSeries_Canada_Train$Tech_Qty <- aggregate(Canada_Consumer$Quantity, by = list(Canada_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Canada_Train$Tech_Profit <- aggregate(Canada_Consumer$Profit, by = list(Canada_Consumer$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

TimeSeries_Canada_Train$Month <- 1:nrow(TimeSeries_Canada_Train)
# View(TimeSeries_Canada_Train)

Canada_Consumer_Profit_Train_covar <- ( 100*sd(TimeSeries_Canada_Train$Tech_Profit)/mean(TimeSeries_Canada_Train$Tech_Profit) )
print(paste("Canada_Consumer_Profit coefficient of variation of the Profit is :", Canada_Consumer_Profit_Train_covar, "%"))

COV_DF <- rbind(COV_DF,data.frame(Names = "Canada_Consumer_Profit", COV = Canada_Consumer_Profit_Train_covar))
View(COV_DF)

# Canada_Corporate

TimeSeries_Canada_Train_2 <- aggregate(Canada_Corporate$Sales, by = list(Canada_Corporate$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_Canada_Train_2) <- c("Month", "Corporate_Sales")
TimeSeries_Canada_Train_2$Corporate_Qty <- aggregate(Canada_Corporate$Quantity, by = list(Canada_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Canada_Train_2$Corporate_Profit <- aggregate(Canada_Corporate$Profit, by = list(Canada_Corporate$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_Canada_Train_2)

Canada_Corporate_Profit_Train_covar <- ( 100*sd(TimeSeries_Canada_Train_2$Corporate_Profit)/mean(TimeSeries_Canada_Train_2$Corporate_Profit) )
print(paste("Canada_Corporate_Profit coefficient of variation of the Profit is :", Canada_Corporate_Profit_Train_covar, "%"))
COV_DF<- rbind(COV_DF, data.frame(Names ="Canada_Corporate_Profit", COV = Canada_Corporate_Profit_Train_covar))
View(COV_DF)

# Canada_Home_Office

TimeSeries_Canada_Train_1 <- aggregate(Canada_Home_Office$Sales, by = list(Canada_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)
colnames(TimeSeries_Canada_Train_1) <- c("Month", "Office_supplies_Sales")
TimeSeries_Canada_Train_1$Home_Office_Qty <- aggregate(Canada_Home_Office$Quantity, by = list(Canada_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]
TimeSeries_Canada_Train_1$Home_Office_Profit <- aggregate(Canada_Home_Office$Profit, by = list(Canada_Home_Office$Order.Date), FUN = sum, na.rm = TRUE)[,-1]

# View(TimeSeries_Canada_Train_1)

Canada_Home_Office_Profit_Train_covar <- ( 100*sd(TimeSeries_Canada_Train_1$Home_Office_Profit)/mean(TimeSeries_Canada_Train_1$Home_Office_Profit) )
print(paste("Canada_Home_Office_Profit coefficient of variation of the Profit is :", Canada_Home_Office_Profit_Train_covar, "%"))

COV_DF<- rbind(COV_DF, data.frame(Names ="Canada_Home_Office_Profit", COV = Canada_Home_Office_Profit_Train_covar))
View(COV_DF)

## Plot the COV values and indicate the best sectors.
ggplot(COV_DF, aes(x=Names, y = COV, fill = "green"))+ geom_bar(stat='identity') + coord_flip()+ theme_bw() + geom_text(aes(y = COV/2,label = paste(round(COV,1), "%")))

# 4. Find the 2 most profitable and consistently profitable segments
print(paste("Market and Segment with best COV is :", COV_DF$Names[which(COV_DF$COV == sort(COV_DF$COV, decreasing = FALSE)[1])]))
print(paste("Market and Segment with second best COV is :", COV_DF$Names[which(COV_DF$COV == sort(COV_DF$COV, decreasing = FALSE)[2])]))

# 5. Forecast the sales and quantity for the next 6 months
# 5.1 smoothen the data
# 5.2 classical decomposition
# 5.3 auto ARIMA
# 6. Forecast the sales/demand for next 6 months using this model

Time_Series_EU_Consumer_Sales_Complete <- TimeSeries_EU_Train[,c(1,2)]
Time_Series_EU_Consumer_Sales_Train <- TimeSeries_EU_Train[1:42,c(1,2)]
Time_Series_EU_Consumer_Sales_Test <- TimeSeries_EU_Train[43:48,c(1,2)]
colnames(Time_Series_EU_Consumer_Sales_Test) 

Time_Series_EU_Consumer_Qty_Complete <- TimeSeries_EU_Train[ ,c(1,3)]
Time_Series_EU_Consumer_Qty_Train <- TimeSeries_EU_Train[1:42,c(1,3)]
Time_Series_EU_Consumer_Qty_Test <- TimeSeries_EU_Train[43:48,c(1,3)]
colnames(Time_Series_EU_Consumer_Qty_Test)

Time_Series_APAC_Consumer_Sales_Complete <- TimeSeries_APAC_Train[,c(1,2)]
Time_Series_APAC_Consumer_Sales_Train <- TimeSeries_APAC_Train[1:42,c(1,2)]
Time_Series_APAC_Consumer_Sales_Test <- TimeSeries_APAC_Train[43:48,c(1,2)]
colnames(Time_Series_APAC_Consumer_Sales_Test) 

Time_Series_APAC_Consumer_Qty_Complete <- TimeSeries_APAC_Train[ ,c(1,3)]
Time_Series_APAC_Consumer_Qty_Train <- TimeSeries_APAC_Train[1:42,c(1,3)]
Time_Series_APAC_Consumer_Qty_Test <- TimeSeries_APAC_Train[43:48,c(1,3)]
colnames(Time_Series_APAC_Consumer_Qty_Test)

#loading the file into R

ylab_EU_Consumer_Sales <- c("Sales Registered")
xlab_EU_Consumer_Sales <- c("Months from Jan 2011")
title_EU_Consumer_Sales <- c("EU_Consumer_Sales, Jan. 2011 to Dec 2014")

xcol <- c(1)
ycol <- c(2)

example <- 1

#Let's create the model using the first 42 rows.Then we can test the model on the remaining 6 rows later

total_timeser_EU_Consumer_Sales <- ts(Time_Series_EU_Consumer_Sales_Complete$Consumer_Sales, start=c(2011,1), end=c(2014, 12), frequency = 12)
plot(decompose(total_timeser_EU_Consumer_Sales))

total_timeser_EU_Consumer_Sales <- ts(Time_Series_EU_Consumer_Sales_Complete$Consumer_Sales)
timeser_EU_Consumer_Sales <- ts(Time_Series_EU_Consumer_Sales_Train$Consumer_Sales)
plot(timeser_EU_Consumer_Sales)

# View(Time_Series_EU_Consumer_Sales_Train)

#Smoothing the series - Moving Average Smoothing
detach(package:dplyr)
w <-1
smoothedseries_1 <- filter(timeser_EU_Consumer_Sales, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_1[w+2] - smoothedseries_1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_1[i] <- smoothedseries_1[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_EU_Consumer_Sales)

diff <- smoothedseries_1[n-w] - smoothedseries_1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_1[i] <- smoothedseries_1[i-1] + diff
}


#Plot the smoothed time series

timevals_in_1 <- Time_Series_EU_Consumer_Sales_Train$Month
lines(smoothedseries_1, col="blue", lwd=2)
smoothedseries_1

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_1 <- as.data.frame(cbind(timevals_in_1, as.vector(smoothedseries_1)))
colnames(smootheddf_1) <- c('Month', 'Sales')
View(smootheddf_1)

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

LM_linear <- function(smootheddf) {
  
  lmfit <- lm(smootheddf[,2] ~ Month, data=smootheddf)
  return(lmfit)
}

LM_log <- function(smootheddf) {
  
  lmfit <- lm(smootheddf[,2] ~ log10(Month), data=smootheddf)
  return(lmfit)
}

LM_sino <- function(smootheddf) {
  
  lmfit <- lm(smootheddf[,2] ~ sin(0.5*Month) *
                     poly(Month,2) 
                   + cos(0.5* Month) * 
                     poly(Month,2)
                   + sin(0.05*Month)*
                     Month, 
                   data=smootheddf)
  return(lmfit)
}

LM_sino_log <- function(smootheddf) {
  lmfit <- lm(smootheddf[,2] ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + log10(Month), data=smootheddf)
}

MAPE_func <- function(smootheddf_1,timevals_in_1,timeser_EU_Consumer_Sales,Time_Series_EU_Consumer_Sales_Test,selection) {

  if (selection == 1) {
    lmfit_Try1 <- LM_linear(smootheddf_1)
  } 
  
  if (selection == 2) {
    lmfit_Try1 <- LM_log (smootheddf_1)
  } 
  
  if (selection == 3) {
    lmfit_Try1 <- LM_sino (smootheddf_1)
  }
  
  if (selection == 4) {
    lmfit_Try1 <- LM_sino_log (smootheddf_1)
  }
#lmfit_Try1 <- lm(Sales ~ Month, data=smootheddf_1)

global_pred_Try1 <- predict(lmfit_Try1, Month=timevals_in_1)
lines(timevals_in_1, global_pred_Try1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_1 <- timeser_EU_Consumer_Sales-global_pred_Try1

plot(local_pred_1, col='red', type = "l")
acf(local_pred_1)
acf(local_pred_1, type="partial")
armafit_1 <- auto.arima(local_pred_1)

tsdiag(armafit_1)
armafit_1

#We'll check if the residual series is white noise

resi_1 <- local_pred_1-fitted(armafit_1)
resi_1

adf.test(resi_1,alternative = "stationary")
kpss.test(resi_1)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_1 <- Time_Series_EU_Consumer_Sales_Test
timevals_out_1 <- outdata_1$Month
timevals_out_1

global_pred_out_1 <- predict(lmfit_Try1,data.frame(Month =timevals_out_1))

fcast_1 <- global_pred_out_1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_1 <- accuracy(fcast_1,outdata_1[,2])[5]

return(MAPE_class_dec_1)

}

MAPE_linear <- c(MAPE_func(smootheddf_1,timevals_in_1, timeser_EU_Consumer_Sales, Time_Series_EU_Consumer_Sales_Test,1))
MAPE_linear[2] <- MAPE_func(smootheddf_1,timevals_in_1, timeser_EU_Consumer_Sales, Time_Series_EU_Consumer_Sales_Test,2)
MAPE_linear[3] <- MAPE_func(smootheddf_1,timevals_in_1, timeser_EU_Consumer_Sales, Time_Series_EU_Consumer_Sales_Test,3)
MAPE_linear[4] <- MAPE_func(smootheddf_1,timevals_in_1, timeser_EU_Consumer_Sales, Time_Series_EU_Consumer_Sales_Test,4)
MAPE_linear

print(paste("Linear fit gives the lowest MAPE value"))


lmfit_1 <- LM_linear(smootheddf_1)
global_pred_1 <- predict(lmfit_1, Month=timevals_in_1)
outdata_1 <- Time_Series_EU_Consumer_Sales_Test
timevals_out_1 <- outdata_1$Month
timevals_out_1

global_pred_out_1 <- predict(lmfit_1,data.frame(Month =timevals_out_1))

class_dec_pred_1 <- c(ts(global_pred_1),ts(global_pred_out_1))

plot(total_timeser_EU_Consumer_Sales, col = "black")
lines(class_dec_pred_1, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_1 <- auto.arima(timeser_EU_Consumer_Sales)
autoarima_1
typeof(autoarima_1)
tsdiag(autoarima_1)
autoarima_1$x
plot(autoarima_1$x, col="black")
lines(fitted(autoarima_1), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_1 <- timeser_EU_Consumer_Sales - fitted(autoarima_1)
plot(resi_auto_arima_1)
adf.test(resi_auto_arima_1,alternative = "stationary")
kpss.test(resi_auto_arima_1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_1 <- predict(autoarima_1, n.ahead = 6)
fcast_auto_arima_1
MAPE_auto_arima_1 <- accuracy(fcast_auto_arima_1$pred,outdata_1[,2])[5]
MAPE_auto_arima_1

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
ts(fcast_auto_arima_1$pred)
auto_arima_pred_1 <- c(fitted(autoarima_1),ts(fcast_auto_arima_1$pred))
plot(total_timeser_EU_Consumer_Sales, col = "black")
lines(auto_arima_pred_1, col = "red")

#############################################################################################

ylab_EU_Consumer_Qty <- c("Sales Registered")
xlab_EU_Consumer_Qty <- c("Months from Jan 2011")
title_EU_Consumer_Qty <- c("EU_Consumer_Qty, Jan. 2011 to Dec 2014")

xcol <- c(1)
ycol <- c(2)

example <- 1

#Let's create the model using the first 42 rows.Then we can test the model on the remaining 6 rows later

total_timeser_EU_Consumer_Qty <- ts(Time_Series_EU_Consumer_Qty_Complete$Tech_Qty, start=c(2011,1), end=c(2014, 12), frequency = 12)
plot(decompose(total_timeser_EU_Consumer_Qty))

total_timeser_EU_Consumer_Qty <- ts(Time_Series_EU_Consumer_Qty_Complete$Tech_Qty)

timeser_EU_Consumer_Qty <- ts(Time_Series_EU_Consumer_Qty_Train$Tech_Qty)
plot(timeser_EU_Consumer_Qty)

# View(Time_Series_EU_Consumer_Qty_Train)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries_2 <- filter(timeser_EU_Consumer_Qty, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_2[w+2] - smoothedseries_2[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_2[i] <- smoothedseries_2[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_EU_Consumer_Qty)

diff <- smoothedseries_2[n-w] - smoothedseries_2[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_2[i] <- smoothedseries_2[i-1] + diff
}

#Plot the smoothed time series

timevals_in_2 <- Time_Series_EU_Consumer_Qty_Train$Month
lines(smoothedseries_2, col="blue", lwd=2)
smoothedseries_2

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_2 <- as.data.frame(cbind(timevals_in_2, as.vector(smoothedseries_2)))
colnames(smootheddf_2) <- c('Month', 'Qty')
View(smootheddf_2)

MAPE_EU_Consumer_Qty <- c(MAPE_func(smootheddf_2,timevals_in_2, timeser_EU_Consumer_Qty, Time_Series_EU_Consumer_Qty_Test,1))
MAPE_EU_Consumer_Qty[2] <- MAPE_func(smootheddf_2,timevals_in_2, timeser_EU_Consumer_Qty, Time_Series_EU_Consumer_Qty_Test,2)
MAPE_EU_Consumer_Qty[3] <- MAPE_func(smootheddf_2,timevals_in_2, timeser_EU_Consumer_Qty, Time_Series_EU_Consumer_Qty_Test,3)
MAPE_EU_Consumer_Qty[4] <- MAPE_func(smootheddf_2,timevals_in_2, timeser_EU_Consumer_Qty, Time_Series_EU_Consumer_Qty_Test,4)

MAPE_EU_Consumer_Qty

print(paste("Sinusoidal fit gives the lowest MAPE value"))


lmfit_2 <- LM_sino(smootheddf_2)
global_pred_2 <- predict(lmfit_2, Month=timevals_in_2)
outdata_2 <- Time_Series_EU_Consumer_Qty_Test
timevals_out_2 <- outdata_2$Month
timevals_out_2

global_pred_out_2 <- predict(lmfit_2,data.frame(Month =timevals_out_2))

class_dec_pred_2 <- c(ts(global_pred_2),ts(global_pred_out_2))

plot(total_timeser_EU_Consumer_Qty, col = "black")
lines(class_dec_pred_2, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_2 <- auto.arima(timeser_EU_Consumer_Qty)
autoarima_2
typeof(autoarima_2)
tsdiag(autoarima_2)
autoarima_2$x
plot(autoarima_2$x, col="black")
lines(fitted(autoarima_2), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_2 <- timeser_EU_Consumer_Qty - fitted(autoarima_2)
plot(resi_auto_arima_2)
adf.test(resi_auto_arima_2,alternative = "stationary")
kpss.test(resi_auto_arima_2)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_2 <- predict(autoarima_2, n.ahead = 6)
fcast_auto_arima_2
MAPE_auto_arima_2 <- accuracy(fcast_auto_arima_2$pred,outdata_2[,2])[5]
MAPE_auto_arima_2

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_2 <- c(fitted(autoarima_2),ts(fcast_auto_arima_2$pred))
plot(total_timeser_EU_Consumer_Qty, col = "black")
lines(auto_arima_pred_2, col = "red")

#############################################################################################

ylab_APAC_Consumer_Sales <- c("Sales Registered")
xlab_APAC_Consumer_Sales <- c("Months from Jan 2011")
title_APAC_Consumer_Sales <- c("APAC_Consumer_Sales, Jan. 2011 to Dec 2014")

xcol <- c(1)
ycol <- c(2)

example <- 1

#Let's create the model using the first 42 rows.Then we can test the model on the remaining 6 rows later

total_timeser_APAC_Consumer_Sales <- ts(Time_Series_APAC_Consumer_Sales_Complete$Consumer_Sales, start=c(2011,1), end=c(2014, 12), frequency = 12)
plot(decompose(total_timeser_APAC_Consumer_Sales))

total_timeser_APAC_Consumer_Sales <- ts(Time_Series_APAC_Consumer_Sales_Complete$Consumer_Sales)

timeser_APAC_Consumer_Sales <- ts(Time_Series_APAC_Consumer_Sales_Train$Consumer_Sales)
plot(timeser_APAC_Consumer_Sales)

# View(Time_Series_APAC_Consumer_Sales_Train)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries_3 <- filter(timeser_APAC_Consumer_Sales, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_3[w+2] - smoothedseries_3[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_3[i] <- smoothedseries_3[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_APAC_Consumer_Sales)

diff <- smoothedseries_3[n-w] - smoothedseries_3[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_3[i] <- smoothedseries_3[i-1] + diff
}


#Plot the smoothed time series

timevals_in_3 <- Time_Series_APAC_Consumer_Sales_Train$Month
lines(smoothedseries_3, col="blue", lwd=2)
smoothedseries_3

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_3 <- as.data.frame(cbind(timevals_in_3, as.vector(smoothedseries_3)))
colnames(smootheddf_3) <- c('Month', 'Sales')
View(smootheddf_3)


MAPE_APAC_Consumer_Sales <- c(MAPE_func(smootheddf_3,timevals_in_3, timeser_APAC_Consumer_Sales, Time_Series_APAC_Consumer_Sales_Test,1))
MAPE_APAC_Consumer_Sales[2] <- MAPE_func(smootheddf_3,timevals_in_3, timeser_APAC_Consumer_Sales, Time_Series_APAC_Consumer_Sales_Test,2)
MAPE_APAC_Consumer_Sales[3] <- MAPE_func(smootheddf_3,timevals_in_3, timeser_APAC_Consumer_Sales, Time_Series_APAC_Consumer_Sales_Test,3)
MAPE_APAC_Consumer_Sales[4] <- MAPE_func(smootheddf_3,timevals_in_3, timeser_APAC_Consumer_Sales, Time_Series_APAC_Consumer_Sales_Test,4)

MAPE_APAC_Consumer_Sales

print(paste("Linear fit gives the lowest MAPE value"))


lmfit_3 <- LM_linear(smootheddf_3)
global_pred_3 <- predict(lmfit_3, Month=timevals_in_3)
outdata_3 <- Time_Series_APAC_Consumer_Sales_Test
timevals_out_3 <- outdata_3$Month
timevals_out_3

global_pred_out_3 <- predict(lmfit_3,data.frame(Month =timevals_out_3))

class_dec_pred_3 <- c(ts(global_pred_3),ts(global_pred_out_3))

plot(total_timeser_APAC_Consumer_Sales, col = "black")
lines(class_dec_pred_3, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_3 <- auto.arima(timeser_APAC_Consumer_Sales)
autoarima_3
typeof(autoarima_3)
tsdiag(autoarima_3)
autoarima_3$x
plot(autoarima_3$x, col="black")
lines(fitted(autoarima_3), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_3 <- timeser_APAC_Consumer_Sales - fitted(autoarima_3)
plot(resi_auto_arima_3)
adf.test(resi_auto_arima_3,alternative = "stationary")
kpss.test(resi_auto_arima_3)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_3 <- predict(autoarima_3, n.ahead = 6)
fcast_auto_arima_3
MAPE_auto_arima_3 <- accuracy(fcast_auto_arima_3$pred,outdata_3[,2])[5]
MAPE_auto_arima_3

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_3 <- c(fitted(autoarima_3),ts(fcast_auto_arima_3$pred))
plot(total_timeser_APAC_Consumer_Sales, col = "black")
lines(auto_arima_pred_3, col = "red")

##############################################################################################

ylab_APAC_Consumer_Qty <- c("Sales Registered")
xlab_APAC_Consumer_Qty <- c("Months from Jan 2011")
title_APAC_Consumer_Qty <- c("APAC_Consumer_Qty, Jan. 2011 to Dec 2014")

xcol <- c(1)
ycol <- c(2)

example <- 1

#Let's create the model using the first 42 rows.Then we can test the model on the remaining 6 rows later

total_timeser_APAC_Consumer_Qty <- ts(Time_Series_APAC_Consumer_Qty_Complete$Tech_Qty, start=c(2011,1), end=c(2014, 12), frequency = 12)
plot(decompose(total_timeser_APAC_Consumer_Qty))

total_timeser_APAC_Consumer_Qty <- ts(Time_Series_APAC_Consumer_Qty_Complete$Tech_Qty)

timeser_APAC_Consumer_Qty <- ts(Time_Series_APAC_Consumer_Qty_Train$Tech_Qty)
plot(timeser_APAC_Consumer_Qty)

# View(Time_Series_APAC_Consumer_Qty_Train)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries_4 <- filter(timeser_APAC_Consumer_Qty, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_4[w+2] - smoothedseries_4[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_4[i] <- smoothedseries_4[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_APAC_Consumer_Qty)

diff <- smoothedseries_4[n-w] - smoothedseries_4[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_4[i] <- smoothedseries_4[i-1] + diff
}


#Plot the smoothed time series

timevals_in_4 <- Time_Series_APAC_Consumer_Qty_Train$Month
lines(smoothedseries_4, col="blue", lwd=2)
smoothedseries_4

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_4 <- as.data.frame(cbind(timevals_in_4, as.vector(smoothedseries_4)))
colnames(smootheddf_4) <- c('Month', 'Qty')
View(smootheddf_4)

MAPE_APAC_Consumer_Qty <- c(MAPE_func(smootheddf_4,timevals_in_4, timeser_APAC_Consumer_Qty, Time_Series_APAC_Consumer_Qty_Test,1))
MAPE_APAC_Consumer_Qty[2] <- MAPE_func(smootheddf_4,timevals_in_4, timeser_APAC_Consumer_Qty, Time_Series_APAC_Consumer_Qty_Test,2)
MAPE_APAC_Consumer_Qty[3] <- MAPE_func(smootheddf_4,timevals_in_4, timeser_APAC_Consumer_Qty, Time_Series_APAC_Consumer_Qty_Test,3)
MAPE_APAC_Consumer_Qty[4] <- MAPE_func(smootheddf_4,timevals_in_4, timeser_APAC_Consumer_Qty, Time_Series_APAC_Consumer_Qty_Test,4)

MAPE_APAC_Consumer_Qty

print(paste("Linear fit gives the lowest MAPE value"))


lmfit_4 <- LM_linear(smootheddf_4)
global_pred_4 <- predict(lmfit_4, Month=timevals_in_4)
outdata_4 <- Time_Series_APAC_Consumer_Qty_Test
timevals_out_4 <- outdata_4$Month
timevals_out_4

global_pred_out_4 <- predict(lmfit_4,data.frame(Month =timevals_out_4))

class_dec_pred_4 <- c(ts(global_pred_4),ts(global_pred_out_4))

plot(total_timeser_APAC_Consumer_Qty, col = "black")
lines(class_dec_pred_4, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_4 <- auto.arima(timeser_APAC_Consumer_Qty)
autoarima_4
typeof(autoarima_4)
tsdiag(autoarima_4)
autoarima_4$x
plot(autoarima_4$x, col="black")
lines(fitted(autoarima_4), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_4 <- timeser_APAC_Consumer_Qty - fitted(autoarima_4)
plot(resi_auto_arima_4)
adf.test(resi_auto_arima_4,alternative = "stationary")
kpss.test(resi_auto_arima_4)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_4 <- predict(autoarima_4, n.ahead = 6)
fcast_auto_arima_4
MAPE_auto_arima_4 <- accuracy(fcast_auto_arima_4$pred,outdata_4[,2])[5]
MAPE_auto_arima_4

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_4 <- c(fitted(autoarima_4),ts(fcast_auto_arima_4$pred))
plot(total_timeser_APAC_Consumer_Qty, col = "black")
lines(auto_arima_pred_4, col = "red")

########################## END OF PROGRAM ################################
