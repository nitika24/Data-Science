library(readr)
emp_data <- read_csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\emp_data.csv")
View(emp_data)

# Exploratory data analysis
summary(emp_data)

#Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)  # plot(X,Y)

?plot

attach(emp_data)


#Correlation Coefficient (r)
cor(Salary_hike, Churn_out_rate)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE


sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")


# ggplot for adding regresion line for data
library(ggplot2)


ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))
