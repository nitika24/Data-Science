# Load delivery_time.csv dataset
library(readr)
delivery_time <- read_csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\delivery_time.csv")
View(delivery_time)

# Exploratory data analysis
summary(delivery_time)

#Scatter plot
plot(delivery_time$`Delivery Time`, delivery_time$`Sorting Time`)  # plot(X,Y)

?plot

attach(delivery_time)


#Correlation Coefficient (r)
cor(`Delivery Time`, `Sorting Time`)             # cor(X,Y)

# Simple Linear regrression model
regr <- lm(`Delivery Time` ~ `Sorting Time`) # lm(Y ~ X)

summary(regr)

pred <- predict(regr)

regr$residuals
sum(regr$residuals)

mean(regr$residuals)
sqrt(sum(regr$residuals^2)/nrow(delivery_time))  #RMSE


sqrt(mean(regr$residuals^2))

confint(regr,level=0.95)
predict(regr,interval="predict")

??confint
??predict

# ggplot for adding regrresion line for data
library(ggplot2)

?ggplot2

ggplot(data = delivery_time, aes(x =  `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=`Sorting Time`, y=pred))

#Increasing R value

plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)

reg_log <- lm(`Delivery Time` ~ log(`Sorting Time`))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(delivery_time))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

#Increasing R squared (Exponential Model)

plot(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`, log(`Delivery Time`))

reg_exp <- lm(log(`Delivery Time`) ~ `Sorting Time`)
summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logdeliverytime <- predict(reg_exp)

deliverytime <- exp(logdeliverytime)

error <- delivery_time$`Delivery Time` - deliverytime
error

sqrt(sum(error^2)/nrow(delivery_time)) #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`*`Sorting Time`, `Delivery Time`)

cor(`Sorting Time`*`Sorting Time`, `Delivery Time`)

plot(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(`Delivery Time`) ~ `Sorting Time` + I(`Sorting Time`*`Sorting Time`))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = delivery_time$`Delivery Time` - expy

sqrt(sum(err^2)/nrow(delivery_time))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = delivery_time, aes(x = `Sorting Time` + I(`Sorting Time`^2), y = log(`Delivery Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=`Sorting Time`+I(`Sorting Time`^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(`Delivery Time`)~`Sorting Time` + I(`Sorting Time`*`Sorting Time`) + I(`Sorting Time`*`Sorting Time`*`Sorting Time`))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = delivery_time, aes(x = `Sorting Time` + I(`Sorting Time`^2) + I(`Sorting Time`^3), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=`Sorting Time`+I(`Sorting Time`^2)+I(`Sorting Time`^3), y=expy3))
