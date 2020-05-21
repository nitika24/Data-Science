install.packages("partykit")
library(partykit)

data()
data("iris")
View(iris)
?iris

##### Using D.Tree #####
library(tree)
iris_tree <- ctree(Species~.,data=iris)
?tree
plot(iris_tree)
?text

plot(iris_tree, type = "simple")
