credit <- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\creditcard.csv")
View(credit)


credit[1] = NULL

attach(credit)
str(credit)

#check if there are missing values - since there are none, proceed
summary(credit)

#make training and test sets - used caret package because the usual wacard was giving me a vector for the training set, which is not what I wanted
install.packages("caret")
library(caret)
#set random seed for the results to be reproducible
set.seed(123) 
# sample 75% of observations as the training set
??createDataPartition
trainX <- createDataPartition(card,p=0.75,list=FALSE) 
train <- credit[trainX,]
# the rest 25% as the test set
test <- credit[-trainX,]

#View(train)

qplot(card, age, data=credit, geom="boxplot")
??cardlim
qplot(card, income, data=credit, geom="boxplot", ylim=c(0,30000))
qplot(card, share, data=credit, geom="boxplot") #orig

qplot(card, expenditure, data=credit, geom="boxplot")

qplot(card, owner, data=credit, geom="boxplot")


qplot(card, active, data=credit, geom="boxplot", ylim=c(0,1000)) #diff
qplot(card, active, data=credit, geom="boxplot") #orig



#Logistic Regression
credit_reg <- glm(card~.,data=train[,-11], family=binomial) 
summary(credit_reg)


# confusion matrix table
prob <- round(predict(credit_reg,type="response"), digits=3)
??predict
View(prob)

# store the predicted labels using 0.5 as a threshold
library(dplcardr)
train = train %>%
  mutate(predic=as.factor(ifelse(prob<=0.5, "No", "yes")))
# confusion matrix
table(pred=train$predic, true=train$card)


Prob <- round(predict(credit_reg, test, type="response"),digits=3)
test = test %>%
  mutate(Probabilitcard=Prob)
test

library(ROCR)
pred = prediction(prob, train$card)
# we want TPR on the card axis and FPR on the x axis
perf = performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=2, lwd=3, main="ROC curve")
abline(0,1)

auc <- performance(pred, "auc")@y.values
auc
View(credit)
#optimum for TPR is 0.82. to increase true positive rate, we have to change threshold values
# FPR
fpr = performance(pred, "fpr")@y.values[[1]]
cutoff = performance(pred, "fpr")@x.values[[1]]
# FNR
fnr = performance(pred,"fnr")@y.values[[1]]

# use matplot to plot the FPR and FNR versus threshold values
matplot(cutoff, cbind(fpr,fnr), tcardpe="l",lwd=2, xlab="Threshold",ylab="Error Rate")
# add legend to the plot
legend(0.4, 1, legend=c("False Positive Rate","False Negative Rate"),
       col=c(1,2), lty=c(1,2))

# calculate the euclidean distance between (FPR,FNR) and (0,0)
rate = as.data.frame(cbind(Cutoff=cutoff, FPR=fpr, FNR=fnr))
rate$distance = sqrt((rate[,2])^2+(rate[,3])^2)

# select the probabilitcard threshold with the smallest euclidean distance
index = which.min(rate$distance)
best = rate$Cutoff[index]
best
abline(v=best, col=3, ltcard=3, lwd=3)
#Therefore, our best cutoff value is 0.099 which corresponds to the smallest Euclidean distance 0.20.
#That being said, assigning hiring probabilities less than 0.099 to No and higher than 0.099 to cardes makes
#(FPR, FNR) and (0, 0) closest.

#calculate subscription probabilities again with threshold value
new.train = train %>%
  mutate(predic=as.factor(ifelse(prob<=0.099, "No", "yes")))
# confusion matrix
table(pred=new.train$predic, true=train$card)
#Out of 3391 cases, model classifies 2686 correctlcard (79.2%)
#Out of 3000 not subscribed to a term deposit, model classifies 2360 correctlcard (78.67%)
#Out of 391 subscribed to a term deposit, model classifies 326 correctlcard (83.38%) 
#We see that this is a huge increase in true pos rate and a much more accurate model overall.



