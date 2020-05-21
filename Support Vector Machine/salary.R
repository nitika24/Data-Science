library(kernlab)
library(caret)
library(ggplot2)

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

# Building model 


model1<-ksvm(factor(train_sal$Salary)~., 
             data= train_sal, kernel = "vanilladot")
model1


Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)

agreement <- Salary_prediction == test_sal$Salary
table(agreement)

prop.table(table(agreement))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(factor(train_sal$Salary)~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) # 85.21

# kernel = vanilladot
model_vanilla<-ksvm(factor(train_sal$Salary)~., 
                    data= train_sal,kernel = "vanilladot")


pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) # 84.64


#kernel = besseldot
model_bessel<-ksvm(factor(train_sal$Salary)~., 
                    data= train_sal,kernel = "besseldot")


pred_bessel<-predict(model_bessel,newdata=test_sal)
mean(pred_bessel==test_sal$Salary)

#kernel = polydot
model_poly<-ksvm(factor(train_sal$Salary)~., 
                    data= train_sal,kernel = "polydot")


pred_poly<-predict(model_poly,newdata=test_sal)
mean(pred_poly==test_sal$Salary)

plot(pred_poly)
plot(model_poly,data =train_sal)
