order <- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\Costomer+OrderForm.csv")

View(order) # countries are in their own columns; so we need to stack the data 
stacked_order<-stack(order)
attach(stacked_order)
View(stacked_order)
table(stacked_order$ind,stacked_order$values)
chisq.test(table(stacked_order$ind,stacked_order$values))
