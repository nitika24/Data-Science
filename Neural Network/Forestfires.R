library(neuralnet)
library(plyr)
library(nnet)
library(NeuralNetTools)
library(psych)

# Read the data
fire <- read.csv("D:\\Nitika\\Data Science\\Assignments\\Neural Network\\forestfires.csv")
View(fire)

class(fire)


str(fire)


fire <- as.data.frame(fire)
attach(fire)

# Exploratory data Analysis :



windows()

pairs.panels(fire[c("temp", "RH", "wind", "rain", "area")])

#Visualization

hist(fire$temp, prob = T, breaks = 30)
lines(density(fire$temp))

hist(fire$RH, prob = T, breaks = 30)
lines(density(fire$RH))

hist(fire$wind, prob = T, breaks = 30)
lines(density(fire$wind))

hist(fire$rain, prob = T, breaks = 30)
lines(density(fire$rain))

# The area value has lots of zeros

hist(fire$area)
rug(fire$area)

# Transform the Area value to Y 

fire1 <- mutate(fire, y = log(area + 1))  # default is to the base e, y is lower case
hist(fire1$y)



summary(fire) # Confirms on the different scale and demands normalizing the data.

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

fire$temp = normalize(fire$temp)
fire$RH   = normalize(fire$RH)
fire$wind = normalize(fire$wind)
fire$rain = normalize(fire$rain)


attach(fire)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(fire), replace = TRUE, prob = c(0.7,0.3))
fire_train <- fire[ind==1,]
fire_test  <- fire[ind==2,]


# Creating a neural network model on training data


fire_model <- neuralnet(size_category~temp+rain+wind+RH, data= fire_train)
str(fire_model)
View(fire_model)

plot(fire_model, rep = "best")

summary(fire_model)

par(mar = numeric(4), family = 'serif')
plotnet(fire_model, alpha = 0.6)


# Evaluating model performance

set.seed(12323)
model_results <- compute(fire_model,fire_test[7:10])

predicted_area <- model_results$net.result
str(predicted_area)



# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(fire$area)
str_min <- min(fire$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

# Improve the model performance :
set.seed(12345)
fire_model2 <- neuralnet(size_category~temp+rain+wind+RH, linear.output = F,data= fire_train,hidden = 5, act.fct = "logistic")
plot(fire_model2 ,rep = "best")

summary(fire_model2)

model_results2<-compute(fire_model2,fire_test[7:10])
predicted_area2<-model_results2$net.result


par(mar = numeric(4), family = 'serif')
plotnet(fire_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons  under hidden layer are increased