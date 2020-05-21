install.packages("knitr")
library(party)
library(C50)
library(tree)
library(gmodels)
library(knitr)

FraudCheck <- read.csv("D:\\Nitika\\Data Science\\Assignments\\Decision Trees\\Fraud_Check.csv")
hist(FraudCheck$Taxable.Income)
View(FraudCheck)
colnames(FraudCheck)
names(FraudCheck)[2] <- "MaritalStatus"
names(FraudCheck)[3] <- "TaxableIncome"
names(FraudCheck)[4] <- "CityPopulation"
names(FraudCheck)[5] <- "WorkExperience"
attach(FraudCheck)
View(FraudCheck)
RiskyGood = ifelse(FraudCheck$TaxableIncome<= 30000, "Risky", "Good")
RiskyGoodfac <- as.factor(RiskyGood)
FC = data.frame(FraudCheck,RiskyGoodfac)
# Splitting data into training and testing.
# splitting the data based on Sales
FC[3]=NULL
View(FC)
FCtrain <- FC[1:300,]

FCtest <- FC[301:600,]


###Using Party Function 
??ctree
class(RiskyGoodfac)
class(Undergrad)
FraudCheck_tree <- ctree(RiskyGoodfac ~ factor(Undergrad) + factor(MaritalStatus) + CityPopulation + 
                     WorkExperience + factor(Urban), data = FCtrain)
summary(FraudCheck_tree)
plot(FraudCheck_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

FC_tree <- ctree(RiskyGoodfac ~ factor(Undergrad) + factor(MaritalStatus) + CityPopulation + 
                  WorkExperience + factor(Urban), data = FCtrain)
summary(FC_tree)

plot(FC_tree)

pred_tree <- as.data.frame(predict(FC_tree,FCtest))
pred_tree["final"] <- NULL
pred_test_df <- predict(FC_tree,FCtest)


mean(pred_test_df==FCtest$RiskyGoodfac) # Accuracy = 82 %
CrossTable(FCtest$RiskyGoodfac,pred_test_df)

confusionMatrix(FCtest$RiskyGoodfac,pred_test_df)

