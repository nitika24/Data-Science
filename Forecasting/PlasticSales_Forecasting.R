library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(timeSeries)
library(tseries)
library(fpp2)
library(multilevel)

# import data
sale <- read_csv("D:\\Nitika\\Data Science\\Assignments\\Forecasting\\PlasticSales.csv")

# Pre-Processing of data
year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(sale)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)
sales <-data.frame(year,month,rn=1:nrow(sale),Sales=sale$Sales) 

#Examine Data
str(sales)
head(sales, n = 5)
View(sales)

options(repr.plot.width = 6, repr.plot.height = 3)
ggplot(sales, aes(x = year, y = Sales)) + geom_line() + geom_smooth(method = 'lm') +labs(x = "Time", y = "Monthly Sales")


# convert our sales data to a time series object
salesTS <- ts(sales$Sales, frequency = 12, start = c(1949))
class(salesTS)

options(repr.plot.width = 6, repr.plot.height = 5)
salesDecomp <- decompose(salesTS)
plot(salesDecomp)

#Looking at the decomposition plot, we can see that there is a good upward trend over the last couple of years, and we can see the regular, seasonal component of our data.

# log transform time series data
salesLog <- log(salesTS)

salesLogHW <- HoltWinters(salesLog)
salesLogHW

options(repr.plot.width = 6, repr.plot.height = 4)
plot(salesLogHW)

#Looking at the plot above, we can see that the Holt-Winters prediction (red), quite closely matches our observed data (black). 

# forecast next year's sales
nextYearSales <- forecast(salesLogHW, h=12)
# plot
plot(nextYearSales)

#######################Additive#####################################################3

salead <- window(salesTS, start = 1949 )
fit1 <- hw(salead,seasonal="additive")
#fit2 <- hw(aust,seasonal="multiplicative")
options(repr.plot.width = 6, repr.plot.height = 4)
plot(fit1)

########################Multiplicative#############################################33
salead <- window(salesTS, start = 1949 )
fit2 <- hw(salead,seasonal="multiplicative")
options(repr.plot.width = 6, repr.plot.height = 4)
plot(fit2)


#Plots for additive and multiplicative seasonality
autoplot(salead) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Sales") +
  ggtitle("Plastic Sakes") +
  guides(colour=guide_legend(title="Forecast"))

##########################ARIMA MODEL################################################

# Using Arima Model 
#Import Data
Plastic<-read.csv(file.choose()) # read the Plastics data

#Pre-Processing
year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(Plastic)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)
Plastics <-data.frame(year,month,rn=1:nrow(Plastic),Sales=Plastic$Sales) 
Plastics <- Plastics$Sales
Plastics <- as.ts(Plastics)
View(Plastics)
class(Plastics)

Plastics1 <- ts(Plastics,start=c(1949),end=c(1953,12),frequency=12)

start(Plastics1)

end(Plastics1)

class(Plastics1)

sum(is.na(Plastics1))

summary(Plastics1)

View(Plastics1)

decomdataAd<- decompose(Plastics1, "additive")
plot(decomdataAd)

decomdataM<- decompose(Plastics1, "multiplicative")
plot(decomdataM)
plot(decomdataM$seasonal)
plot(decomdataM$trend)
plot(decomdataM$random)

# EDA on the Original Data
plot(Plastics1)
abline(reg=lm(Plastics1~time(Plastics1)))

cycle(Plastics1)

# Boxplot by Cycle
boxplot(Plastics1~cycle(Plastics1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))

# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Plastics1)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.

auto.arima(Plastics1, ic = "aic", trace = TRUE)

# tseries evaluation

plot.ts(Newmodel$residuals)

acf(ts(Newmodel$residuals),main = 'ACF Residual')

pacf(ts(Newmodel$residuals),main = 'PACF Residual')

# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)

plot(Pass_Forecast)

# Test your final model

Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")

Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")

Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")

##############################HOLT LINEAR METHOD######################################


# import data
plasticsale <- read_csv("D:\\Nitika\\Data Science\\Assignments\\Forecasting\\PlasticSales.csv")
# examine dataset
year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(plasticsale)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

plasticsales <-data.frame(year,month,rn=1:nrow(plasticsale),Sales=plasticsale$Sales) 
str(plasticsales)
head(plasticsales, n = 5)
View(plasticsales)
??ts
ps<-ts(plasticsales$Sales,frequency =1 ,start=1949)
View(ps)

# dividing entire data into training and testing data 
train<-ps[1:48]
test<-ps[49:60] 
# seasonal data

# converting time series object
train<-ts(train,frequency = 1)
test<-ts(test,frequency = 1)

# Plotting time series data
plot(ps) # Visualization shows that it has level, trend, seasonality => Additive seasonality


logSales <- log(train)
LogSalesHolt <- holt(logSales, alpha = 0.2, beta = 0.1)
LogSalesHolt
holtab_pred<-data.frame(predict(LogSalesHolt,h=4))
options(repr.plot.width = 6, repr.plot.height = 15)
plot(LogSalesHolt)

#Looking at the plot above, we can see that the Holt-Winters prediction (red), quite closely matches our observed data (black). 

# plot
# forecast next quarter sales
nextSales <- forecast(LogSalesHolt, h=4)
plot(nextSales)



################################Moving Averages #######################################
# import data
plasticsale <- read_csv("D:\\Nitika\\Data Science\\Assignments\\Forecasting\\PlasticSales.csv")
# examine dataset
View(plasticsale)
year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(plasticsale)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

plasticsales <-data.frame(year,month,rn=1:nrow(plasticsale),Sales=plasticsale$Sales) 
str(plasticsales)
head(plasticsales, n = 5)
View(plasticsales)
plasticsalesTS <- ts(plasticsales$Sales, frequency = 12, start = c(1949))

options(repr.plot.width = 6, repr.plot.height = 15)
plot(plasticsalesTS)

plascticSales2 <- window(plasticsalesTS,start=1949)
ma4 <- ma(plascticSales2, order=4, centre=FALSE)
ma2x4 <- ma(plascticSales2, order=4, centre=TRUE)

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("year") + ylab("Sales") +
  ggtitle("Plastic Sales") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

nextSales <- forecast(ma2x4, h=12)
plot(nextSales)
