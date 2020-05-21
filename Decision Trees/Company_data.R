library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)

CompanyData <- read.csv("D:\\Nitika\\Data Science\\Assignments\\Decision Trees\\Company_Data.csv")

hist(CompanyData$Sales)

median(CompanyData$Sales)
mean(CompanyData$Sales)
High <- ifelse(CompanyData$Sales<7.49, "No", "Yes")
HighFac <- as.factor(High)
CD <- data.frame(CompanyData, HighFac)
#CD <- CompanyData[,2:12]
# View(CD)

CD_train <- CD[1:200,]

# View(CD_train)
CD_test <- CD[201:400,]

# View(CD_test)

#Using Party Function 
op_tree = ctree(HighFac ~ CompPrice + Income + Advertising + Population + Price + factor(ShelveLoc)
                + Age + Education + factor(Urban)+ factor(US), data = CD_train)
summary(op_tree)

plot(op_tree)

# On looking into the Above tree, i see that if the price is less than 90 
# then there is a probability of 60% chance that the customer will buy.
# and if the location of the shelve is good then there is around 50% chance customer will buy.
# With ShelveLoc having a Bad or Medium and Price <= 87, the probability of High sales 
# could be 60%.
# If ShelveLoc is Bad or Medium, With Price >= 124, around 50% customer will buy 
# And if competitor price is greater than 121 then more than 50% chance that customer will buy.

pred_tree <- as.data.frame(predict(op_tree,CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,CD_test)


mean(pred_test_df==CD$HighFac) # Accuracy = 62.75%
View(CD)
CrossTable(CD_test$HighFac,pred_test_df)
confusionMatrix(CD_test$HighFac,pred_test_df)

confusionMatrix(CD_test$HighFac,pred_test_df)


##### Using tree function 
cd_tree_org <- tree(HighFac~.-Sales,data=CD)
summary(cd_tree_org)

plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

# Using the training data

##### Using tree function 
cd_tree <- tree(HighFac~.-Sales,data=CD_train)
summary(cd_tree)

plot(cd_tree)
text(cd_tree,pretty = 0)

### Evaluate the Model

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,CD_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)

summary(CD_test$HighFac)

mean(pred_tree$final==CD$HighFac) # Accuracy = 58.75
CrossTable(CD_test$HighFac,pred_tree$final)

confusionMatrix(CD_test$HighFac,pred_tree$final)

