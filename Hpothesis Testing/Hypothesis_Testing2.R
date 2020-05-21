
labtat <- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\LabTAT.csv")
attach(labtat)
View(labtat)
stacked_Data <-stack(labtat)
attach(stacked_Data)
View(stacked_Data)

shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

#Variance Test
library(car)
leveneTest(values ~ ind, data = stacked_Data)


