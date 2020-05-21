# Libraries
install.packages("naivebayes")
install.packages("psych")
library(naivebayes)
library(ggplot2)
library(caret)
library(psych)
library(e1071)

# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(factor(train_sal$workclass), factor(train_sal$Salary))

plot(factor(train_sal$education),factor(train_sal$Salary))

plot(factor(train_sal$educationno),factor(train_sal$Salary))

plot(factor(train_sal$maritalstatus), factor(train_sal$Salary))

plot(factor(train_sal$occupation), factor(train_sal$Salary))

plot(factor(train_sal$relationship), factor(train_sal$Salary))

plot(factor(train_sal$race), factor(train_sal$Salary))

plot(factor(train_sal$sex), factor(train_sal$Salary))


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(factor(train_sal$native),factor(train_sal$Salary))


#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Age - Density Plot")

ggplot(data=train_sal,aes(x = train_sal$workclass, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Workclass Density Plot")

ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("education Density Plot")

ggplot(data=train_sal,aes(x = train_sal$educationno, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("educationno Density Plot")

ggplot(data=train_sal,aes(x = train_sal$maritalstatus, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("maritalstatus Density Plot")

ggplot(data=train_sal,aes(x = train_sal$occupation, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("occuptation Density plot")

ggplot(data=train_sal,aes(x = train_sal$sex, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("sex Density plot")

ggplot(data=train_sal,aes(x = train_sal$relationship, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("relationship Density plot")

ggplot(data=train_sal,aes(x = train_sal$race, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("race Density plot")

ggplot(data=train_sal,aes(x = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capital gain Density plot")

ggplot(data=train_sal,aes(x = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Capital loss Density plot")

ggplot(data=train_sal,aes(x = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Hours per week Density plot")

ggplot(data=train_sal,aes(x = train_sal$native, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("Native Density plot")

# Naive Bayes Model 
Model <- naiveBayes(factor(train_sal$Salary) ~ ., data = train_sal)
Model

Model_pred <- factor(predict(Model,test_sal))
mean(Model_pred==test_sal$Salary)

confusionMatrix(table(Model_pred,factor(test_sal$Salary)))

table(Model_pred)
prop.table(table(Model_pred))

library(gmodels)
CrossTable(Model_pred, test_sal$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

#Accuracy of 81 %