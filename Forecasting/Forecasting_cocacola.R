library(readr)
library(xlsx)
cocacola <- read.xlsx("D:\\Nitika\\Data Science\\Assignments\\Forecasting\\CocaCola_Sales_Rawdata.xlsx",1) # read the cocacola data
View(cocacola) # Seasonality 12 months
plot(cocacola$Sales,type = 'o')

# Pre Processing

Q1 <-  ifelse(grepl("Q1",cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cocacola$Quarter),'1','0')

# So creating 4 dummy variables 
cocacolaSales <- cbind(cocacola,Q1,Q2,Q3,Q4)
View(cocacolaSales)
colnames(cocacolaSales)

# input t
cocacolaSales["t"] <- c(1:42)
View(cocacolaSales)

cocacolaSales["log_Sales"] <- log(cocacolaSales["Sales"])
cocacolaSales["t_square"] <- cocacolaSales["t"]*cocacolaSales["t"]
View(cocacolaSales)
## Preprocesing completed

attach(cocacolaSales)
# partitioning
train <- cocacolaSales[1:36,]
test <- cocacolaSales[36:42,]

########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

write.csv(cocacolaSales, file="cocacolaSales.csv", row.names = F)

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############

Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Q1+Q2+Q3+Q$, data = cocacolaSales)
summary(Add_sea_Quad_model_final)
new_model_pred<-data.frame(predict(Add_sea_Quad_model_final,newdata=cocacolaSales,interval='predict'))

new_model_final <- Add_sea_Quad_model$fitted.values

View(new_model_final)



####################### Predicting new data #############################




plot(Add_sea_Quad_model_final)

acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(cocacolaSales$Quarter)

Final <- as.data.frame(cbind(Quarter,cocacolaSales$Sales,new_model_final))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 

plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

View(Final)

