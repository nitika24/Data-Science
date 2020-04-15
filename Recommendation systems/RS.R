
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#movie rating data
book <- read.csv(file.choose())
book[1]=NULL

View(book)
#metadata about the variable
str(book)


#rating distribution
hist(book$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_matrix <- as(book, 'realRatingMatrix')
View(book_matrix)

#Popularity based 

book_model <- Recommender(book_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_model, book_matrix[212:214], n=10)
as(recommended_items1, "list")


## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_model2 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book_model2, book_matrix[212:214], n=10)
as(recommended_items2, "list")


#Item based collabrative filtering

