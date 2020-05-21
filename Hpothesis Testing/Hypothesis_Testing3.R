Buyers<- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\BuyerRatio.csv")
Buyers2<-Buyers[,-1]
attach(Buyers)
chisq.test(Buyers2)
