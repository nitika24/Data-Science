library(arules)
library(arulesViz)
book<-read.csv(file.choose())


rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules

inspect(head(sort(rules, by = "lift"))) 
head(quality(rules))
oneRule <- sample(rules,1)

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(oneRule,method = "mosaic", data = book) #Mosaic only works for one rule

plot(rules,method = "matrix")
plot(rules,method = "matrix3D")
plot(oneRule,method = "doubledecker", data = book) # doubledecker works only for one rule
plot(rules,method = "paracoord")


rules1 <- apriori(as.matrix(book),parameter=list(support=0.05, confidence =0.5,minlen=5))
rules1

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules1))
oneRule1 <- sample(rules1,1)

plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(oneRule1,method = "mosaic", data = book) #Mosaic only works for one rule

plot(rules1,method = "matrix")
plot(rules1,method = "matrix3D")
plot(oneRule1,method = "doubledecker", data = book) # doubledecker works only for one rule
plot(rules1,method = "paracoord")


rules2 <- apriori(as.matrix(book),parameter=list(support= 0.04, confidence =0.9,minlen=5))
rules2

inspect(head(sort(rules1, by = "lift"))) 
head(quality(rules2))
oneRule2 <- sample(rules2,1)

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(oneRule2,method = "mosaic", data = book) #Mosaic only works for one rule

plot(rules2,method = "matrix")
plot(rules2,method = "matrix3D")
plot(oneRule2,method = "doubledecker", data = book) # doubledecker works only for one rule
plot(rules2,method = "paracoord")


#Inference
#The higher the support value and higher the confidence lower are the rules.
#Higher support value than 0.04 and confidence grater than 0.9 is always giving 0 set of rules