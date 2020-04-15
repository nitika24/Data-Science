bank <- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\bank-full.csv", sep = ";")
View(bank)
attach(bank)


str(bank)

#check if there are missing values - since there are none, proceed
summary(bank)

#make training and test sets - used caret package because the usual way was giving me a vector for the training set, which is not what I wanted
install.packages("caret")
library(caret)
#set random seed for the results to be reproducible
set.seed(123) 
# sample 75% of observations as the training set
??createDataPartition
trainX <- createDataPartition(y,p=0.75,list=FALSE) 
train <- bank[trainX,]
# the rest 25% as the test set
test <- bank[-trainX,]

#View(train)

qplot(y, age, data=bank, geom="boxplot")
??ylim
qplot(y, balance, data=bank, geom="boxplot", ylim=c(0,30000))
qplot(y, balance, data=bank, geom="boxplot") #orig

qplot(y, day, data=bank, geom="boxplot")

qplot(y, duration, data=bank, geom="boxplot")

qplot(y, campaign, data=bank, geom="boxplot") #minor difference

qplot(y, pdays, data=bank, geom="boxplot", ylim=c(0,1000)) #diff
qplot(y, pdays, data=bank, geom="boxplot") #orig

qplot(y, previous, data=bank, geom="boxplot") #bad visualization

prev <- table(previous, y)
??barplot
barplot(prev, main="Subscription Based on Previous Number of Contacts Performed",
        xlab="Previous Number of Contacts", col=c("purple"),
        beside=TRUE, ylim=c(0,3500))
#People more likely to say no than yes overall, which is expected.
#But there are fewer people who declined to subscribe the more they were contacted
#and fewer people who subscribed the more they were contacted - which does not reveal any useful insight.


#Logistic Regression
bank_reg <- glm(y~.,data=train[,-11], family=binomial) #take away 11th column
summary(bank_reg)


# confusion matrix table
prob <- round(predict(bank_reg,type="response"), digits=3)
??predict
View(prob)

# store the predicted labels using 0.5 as a threshold
library(dplyr)
train = train %>%
  mutate(predic=as.factor(ifelse(prob<=0.5, "No", "Yes")))
# confusion matrix
table(pred=train$predic, true=train$y)
#Out of 3391 cases, model classifies 3062 correctly (90.3%)
#Out of 3000 not subscribed to a term deposit, model classifies 2943 correctly (98.1%)
#Out of 391 subscribed to a term deposit, model classifies 119 correctly (30.43%)

Prob <- round(predict(bank_reg, test, type="response"),digits=3)
test = test %>%
  mutate(Probability=Prob)
test

#ROC curve because we see that there's small probability of determining accurate results for subscriptions
#low true positive rate
library(ROCR)
pred = prediction(prob, train$y)
# we want TPR on the y axis and FPR on the x axis
perf = performance(pred, measure="tpr", x.measure="fpr")
plot(perf, col=2, lwd=3, main="ROC curve")
abline(0,1)

auc <- performance(pred, "auc")@y.values
auc

#optimum for TPR is 0.82. to increase true positive rate, we have to change threshold values
# FPR
fpr = performance(pred, "fpr")@y.values[[1]]
cutoff = performance(pred, "fpr")@x.values[[1]]
# FNR
fnr = performance(pred,"fnr")@y.values[[1]]

# use matplot to plot the FPR and FNR versus threshold values
matplot(cutoff, cbind(fpr,fnr), type="l",lwd=2, xlab="Threshold",ylab="Error Rate")
# add legend to the plot
legend(0.4, 1, legend=c("False Positive Rate","False Negative Rate"),
       col=c(1,2), lty=c(1,2))

# calculate the euclidean distance between (FPR,FNR) and (0,0)
rate = as.data.frame(cbind(Cutoff=cutoff, FPR=fpr, FNR=fnr))
rate$distance = sqrt((rate[,2])^2+(rate[,3])^2)

# select the probability threshold with the smallest euclidean distance
index = which.min(rate$distance)
best = rate$Cutoff[index]
best
abline(v=best, col=3, lty=3, lwd=3)
#Therefore, our best cutoff value is 0.099 which corresponds to the smallest Euclidean distance 0.20.
#That being said, assigning hiring probabilities less than 0.099 to No and higher than 0.099 to Yes makes
#(FPR, FNR) and (0, 0) closest.

#calculate subscription probabilities again with threshold value
new.train = train %>%
  mutate(predic=as.factor(ifelse(prob<=0.099, "No", "Yes")))
# confusion matrix
table(pred=new.train$predic, true=train$y)
#Out of 3391 cases, model classifies 2686 correctly (79.2%)
#Out of 3000 not subscribed to a term deposit, model classifies 2360 correctly (78.67%)
#Out of 391 subscribed to a term deposit, model classifies 326 correctly (83.38%) 
#We see that this is a huge increase in true pos rate and a much more accurate model overall.



