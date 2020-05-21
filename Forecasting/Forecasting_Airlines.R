library(readr)
library(xlsx)
airlines <- read.xlsx("D:\\Nitika\\Data Science\\Assignments\\Forecasting\\Airlines+Data.xlsx",1) # read the airlines data
View(airlines) # Seasonality 12 months
plot(airlines$Passengers,type = 'o')

# Pre Processing

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X)<-month.abb # Assigning month names 
AirlinesData <- cbind(airlines,X)
View(AirlinesData)
colnames(AirlinesData)

# input t
AirlinesData["t"] <- c(1:96)
View(AirlinesData)

AirlinesData["log_Passengers"] <- log(AirlinesData["Passengers"])
AirlinesData["t_square"] <- AirlinesData["t"]*AirlinesData["t"]
View(AirlinesData)
## Preprocesing completed

attach(AirlinesData)
# partitioning
train <- AirlinesData[1:84,]
test <- AirlinesData[85:96,]

########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

write.csv(AirlinesData, file="AirlinesData.csv", row.names = F)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=AirlinesData)

summary(Add_sea_Quad_model_final)
new_model_pred<-data.frame(predict(Add_sea_Quad_model_final,newdata=AirlinesData,interval='predict'))

new_model_final <- Add_sea_Quad_model_final$fitted.values

View(new_model_final)



####################### Predicting new data #############################




plot(Add_sea_Quad_model_final)

acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(AirlinesData$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_final))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)
plot(Final$Passengers,main = "ActualGraph", xlab="Passengers(Actual)", ylab="Month",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Passengers(Predicted)", ylab="Month",
     col.axis="Green",type="s")

View(Final)
