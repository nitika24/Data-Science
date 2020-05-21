fanta<-read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\Faltoons.csv")
View(fanta)
attach(fanta)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

## P>0.05 accept null hypothesis
##i.e equal proportions


