# Load calories_consumed.csv dataset
library(readr)
calories_consumed <- read_csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\calories_consumed.csv")
View(calories_consumed)

# Exploratory data analysis
summary(calories_consumed)

#Scatter plot
plot(calories_consumed$`Weight gained (grams)`, calories_consumed$`Calories Consumed`)  # plot(X,Y)

?plot

attach(calories_consumed)


#Correlation Coefficient (r)
cor(`Weight gained (grams)`, `Calories Consumed`)             # cor(X,Y)

# Simple Linear regrression model
regr <- lm(`Weight gained (grams)` ~ `Calories Consumed`) # lm(Y ~ X)

summary(regr)

pred <- predict(regr)

regr$residuals
sum(regr$residuals)

mean(regr$residuals)
sqrt(sum(regr$residuals^2)/nrow(calories_consumed))  #RMSE


sqrt(mean(regr$residuals^2))

confint(regr,level=0.95)
predict(regr,interval="predict")

??confint
??predict

# ggplot for adding regrresion line for data
library(ggplot2)

?ggplot2

ggplot(data = calories_consumed, aes(x =  `Calories Consumed`, y = `Weight gained (grams)`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=`Calories Consumed`, y=pred))

?ggplot2