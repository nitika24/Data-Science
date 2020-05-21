library(kernlab)
library(caret)
library(plyr)

# Read the data
Forestfires <- read.csv("D:\\Nitika\\Data Science\\Assignments\\Support Vector Machine\\forestfires.csv")
View(Forestfires)
class(Forestfires)
str(Forestfires)

# The area value has lots of zeros

hist(Forestfires$area)
rug(Forestfires$area)

# Transform the Area value to Y 

Forestfires1 <- mutate(Forestfires, y = log(area + 1))  # default is to the base e, y is lower case
hist(Forestfires1$y)

summary(Forestfires) # Confirms on the diForestfireserent scale and demands normalizing the data.

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Forestfires$temp = normalize(Forestfires$temp)
Forestfires$RH   = normalize(Forestfires$RH)
Forestfires$wind = normalize(Forestfires$wind)
Forestfires$rain = normalize(Forestfires$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :

attach(Forestfires)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Forestfires), replace = TRUE, prob = c(0.7,0.3))
Forestfires_train <- Forestfires[ind==1,]
Forestfires_test  <- Forestfires[ind==2,]
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

# kvsm() function uses gaussian RBF kernel 

# Building model 


model1<-ksvm(factor(size_category)~temp+rain+wind+RH, 
             data= Forestfires_train,kernel = "vanilladot")

model1

Area_pred <- predict(model1, Forestfires_test)

table(Area_pred,Forestfires_test$size_category)

agreement <- Area_pred == Forestfires_test$size_category
table(agreement)

prop.table(table(agreement))

# DiForestfireserent types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(factor(size_category)~temp+rain+wind+RH, 
                  data= Forestfires_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=Forestfires_test)
mean(pred_rfdot==Forestfires_test$size_category) # 68.41

# kernel = vanilladot
model_vanilla<-ksvm(factor(size_category)~temp+rain+wind+RH, 
                    data= Forestfires_train,kernel = "vanilladot")

pred_vanilla<-predict(model_vanilla,newdata=Forestfires_test)
mean(pred_vanilla==Forestfires_test$size_category) # 67.80

# kernal = besseldot
model_besseldot<-ksvm(factor(size_category)~temp+rain+wind+RH, 
                      data= Forestfires_train,kernel = "besseldot")

pred_bessel<-predict(model_besseldot,newdata=Forestfires_test)
mean(pred_bessel==Forestfires_test$size_category) # 67.80

# kernel = polydot

model_poly<-ksvm(factor(size_category)~temp+rain+wind+RH, 
                 data= Forestfires_train,kernel = "polydot")

pred_poly<-predict(model_poly,newdata = Forestfires_test)
mean(pred_poly==Forestfires_test$size_category) # 67.80
