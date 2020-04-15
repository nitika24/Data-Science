library(readr)
salary_data <- read_csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\Salary_data.csv")
View(salary_data)

# Exploratory data analysis
summary(salary_data)

#Scatter plot
plot(salary_data$YearsExperience, salary_data$Salary)

?plot

attach(salary_data)


#Correlation Coefficient (r)
cor(Salary, YearsExperience)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(salary_data))  #RMSE


sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")


# ggplot for adding regresion line for data
library(ggplot2)


ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=pred))
