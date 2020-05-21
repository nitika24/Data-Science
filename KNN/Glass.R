install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot
install.packages("e1071")


library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)
library(e1071)

glass <- read.csv("D:\\Nitika\\Data Science\\Assignments\\KNN\\glass.csv")

#Standardize the data

standard.features <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])
#Check if there are any missing values to impute. 
anyNA(data)
View(data)
head(data)

#Data Visualization
corrplot(cor(data))

#Test and Train data split

set.seed(100)

sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)

#KNN Model

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,factor(test$Type))

#The above result showd that our model achieved accuracy of 70.07 %.
#Trying different K values

predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))


#Choosing K Value by Visualization
#Lets plot error.type vs k using ggplot.

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#The above plot reveals that error is lowest when k=3 and then jumps back high revealing that k=3 is the optimum value. Now lets build our model using k=3 and assess it.

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,factor(test$Type))
                
#The Above Model gave us an accuracy of 76.92 %.
       