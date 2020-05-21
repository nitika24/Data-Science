library(neuralnet)
library(plyr)
library(nnet)
library(NeuralNetTools)

# Read the data
concrete <- read.csv(file.choose())
View(concrete)
class(concrete)


str(concrete)


concrete <- as.data.frame(concrete)
attach(concrete)

# Exploratory data Analysis :

plot(cement, strength)
plot(slag, strength)
plot(ash, strength)
plot(superplastic, strength)
plot(coarseagg, strength)
plot(fineagg, strength)
plot(age,strength)

windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(concrete)
pairs.panels(concrete[c("cement", "slag", "ash", "strength")])

#Visualization

hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))

hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))

hist(concrete$ash, prob = T, breaks = 30)
lines(density(concrete$ash))

hist(concrete$water, prob = T, breaks = 30)
lines(density(concrete$water))

hist(concrete$water, prob = T, breaks = 30)
lines(density(concrete$water))

hist(concrete$superplastic, prob = T, breaks = 30)
lines(density(concrete$superplastic))

hist(concrete$coarseagg, prob = T, breaks = 30)
lines(density(concrete$coarseagg))

hist(concrete$fineagg, prob = T, breaks = 30)
lines(density(concrete$fineagg))

hist(concrete$age, prob = T, breaks = 30)
lines(density(concrete$age))


# Correlation coefficient - Strength & Direction of correlation
cor(concrete)

summary(concrete) # Confirms on the different scale and demands normalizing the data.

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$Profit) # Normalized form of profit

summary(concrete$profit) # Orginal profit value

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_norm[ind==1,]
concrete_test  <- concrete_norm[ind==2,]


# Creating a neural network model on training data


concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train)
str(concrete_model)

plot(concrete_model, rep = "best")

summary(concrete_model)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model, alpha = 0.6)


# Evaluating model performance

set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_strength,concrete_test$strength)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(concrete$strength)
str_min <- min(concrete$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(ActualStrength_pred)

# Improve the model performance :
set.seed(12345)
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = 5, act.fct = "logistic")
plot(concrete_model2 ,rep = "best")

summary(concrete_model2)

model_results2<-compute(concrete_model2,concrete_test[1:8])
predicted_Strenght2<-model_results2$net.result
cor(predicted_Strenght2,concrete_test$strength)

plot(predicted_Strenght2,concrete_test$strength)

par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)

 #The correlation of predicted vs. actual values of the concrete strength is 90.80% more correlated compared to the previous model.

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased