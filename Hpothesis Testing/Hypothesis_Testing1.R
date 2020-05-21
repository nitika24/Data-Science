cutlets<-read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\Cutlets.csv")
attach(cutlets)


## Normality Test

shapiro.test(Unit.A)##P >0.05 Data normal
shapiro.test(Unit.B)##P >0.05 Data normal


## Variance test
var.test(Unit.A,Unit.B) ##P>0.05 ,so p high null fly => Equal variances


## 2 sample t test
t.test(Unit.A,Unit.B, alternative = "two.sided",conf.level = 0.95,correct = TRUE)
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4562 > 0.05 accept null Hypothesis 
# equal means