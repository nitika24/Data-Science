library(arules)
movies<-read.transactions(file.choose(),format="basket")
inspect(movies[1:10])
class(movies)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(movies,topN=20)
movies_rules<-apriori(movies,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)
oneRule <- sample(movies_rules,1)

plot(movies_rules,method = "scatterplot")
plot(movies_rules,method = "grouped")
plot(movies_rules,method = "graph")
plot(oneRule,method = "mosaic", data = movies) #Mosaic only works for one rule

plot(movies_rules,method = "matrix")
plot(movies_rules,method = "matrix3D")
plot(oneRule,method = "doubledecker", data = movies) # doubledecker works only for one rule
plot(movies_rules,method = "paracoord")


rules1 <- apriori(movies,parameter=list(support=0.05, confidence =0.5,minlen=5))
rules1

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules1))
oneRule1 <- sample(rules1,1)

plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(oneRule1,method = "mosaic", data = movies) #Mosaic only works for one rule

plot(rules1,method = "matrix")
plot(rules1,method = "matrix3D")
plot(oneRule1,method = "doubledecker", data = movies) # doubledecker works only for one rule
plot(rules1,method = "paracoord")


rules2 <- apriori(movies,parameter=list(support= 0.09, confidence= 0.9 ,minlen=5))
rules2

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules2))
oneRule2 <- sample(rules2,1)

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(oneRule2,method = "mosaic", data = movies) #Mosaic only works for one rule

plot(rules2,method = "matrix")
plot(rules2,method = "matrix3D")
plot(oneRule2,method = "doubledecker", data = movies) # doubledecker works only for one rule
plot(rules2,method = "paracoord")


#Inference
#The higher the support value and higher the confidence lower are the rules.
#Higher support value than 0.09 and confidence grater than 0.9 is always giving 0 set of rules

