library(arules)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
groceries_rules

library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(groceries_rules,method = "mosaic")


### On inbuilt Data set #####
library(arules)
data("Groceries")
summary(Groceries)
inspect(Groceries[1:10])
??apriori
rules <- apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=4))
rules
inspect(rules[1:5])
windows()
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

rules <- sort(rules,by="lift")

inspect(rules[1:4])


rules1 <- apriori(groceries,parameter=list(support=0.003, confidence =0.05,minlen=2))
rules1

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules1))
oneRule1 <- sample(rules1,1)

plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(oneRule1,method = "mosaic", data = groceries) #Mosaic only works for one rule

plot(rules1,method = "matrix")
plot(rules1,method = "matrix3D")
plot(oneRule1,method = "doubledecker", data = groceries) # doubledecker works only for one rule
plot(rules1,method = "paracoord")


rules2 <- apriori(groceries,parameter=list(support= 0.03, confidence =0.5,minlen=2))
rules2

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules2))
oneRule2 <- sample(rules2,1)

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(oneRule2,method = "mosaic", data = groceries) #Mosaic only works for one rule

plot(rules2,method = "matrix")
plot(rules2,method = "matrix3D")
plot(oneRule2,method = "doubledecker", data = groceries) # doubledecker works only for one rule
plot(rules2,method = "paracoord")


#Inference
#The higher the support value and higher the confidence lower are the rules.
#Support>0.02, confidence>0.05 and minlen>4 gives zero rules.
#If we decrease the minlen and increase the support and confidence, rules will be greater than 0 but upto a particular values.