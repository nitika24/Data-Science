
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
hist(as.vector(as.matrix(book$Book.Rating)), main = "Distribution of book Ratings",
     col = "red", xlab = "Ratings")

boxplot(as.vector(as.matrix(book$Book.Rating)), col = "red", main = "Distribution of book Ratings", ylab = "Ratings")

summary(as.vector(as.matrix(book$Book.Rating)))

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_matrix <- as(book, 'realRatingMatrix')
View(book_matrix)

#Popularity based 

book_model <- Recommender(book_matrix, method="POPULAR")

#Predictions for two users 
recommended_items2 <- predict(book_model, book_matrix[212:214], n=10)
as(recommended_items2, "list")

hist(as.vector(as(recommended_items2,"matrix")), main = "Distribution of book Ratings",
     col = "red", xlab = "Ratings")
#train UBCF cosine similarity models

# non-normalized
UBCF_N_C <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = NULL, method="Cosine"))


# centered
UBCF_C_C <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "center",method="Cosine"))

# Z-score normalization
UBCF_Z_C <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "Z-score",method="Cosine"))

??predict
# compute predicted ratings
p1 <- predict(UBCF_N_C,book_matrix[200:400]) 

p2 <- predict(UBCF_C_C, book_matrix[200:400])

p3 <- predict(UBCF_Z_C, book_matrix[200:400])

View(p1)

boxplot(as.vector(as(p3, "matrix")), col = "green", main = "Distribution of Predicted Values for UBCF Z-Score/Cosine Model", ylab = "Ratings")


hist(as.vector(as(p3, "matrix")), main = "Distrib. of Predicted Values for UBCF Z-Score/Cosine Model", col = "green", xlab = "Predicted Ratings")

as(p3,"list")

#train UBCF Euclidean Distance models

# non-normalized
UBCF_N_E <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = NULL, method="Euclidean"))

# centered
UBCF_C_E <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "center",method="Euclidean"))

# Z-score normalization
UBCF_Z_E <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "Z-score",method="Euclidean"))

# compute predicted ratings
p4 <- predict(UBCF_N_E, book_matrix[200:250])

p5 <- predict(UBCF_C_E, book_matrix[200:250])

p6 <- predict(UBCF_Z_E, book_matrix[200:250])

boxplot(as.vector(as(p6, "matrix")), col = "blue", main = "Distribution of Predicted Values for UBCF Z-Score/Cosine Model", ylab = "Ratings")


hist(as.vector(as(p6, "matrix")), main = "Distrib. of Predicted Values for UBCF Z-Score/Cosine Model", col = "blue", xlab = "Predicted Ratings")


#train UBCF pearson correlation models

# non-normalized
UBCF_N_P <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = NULL, method="pearson"))

# centered
UBCF_C_P <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "center",method="pearson"))

# Z-score normalization
UBCF_Z_P <- Recommender(book_matrix, "UBCF", 
                        param=list(normalize = "Z-score",method="pearson"))

# compute predicted ratings
p7 <- predict(UBCF_N_P, book_matrix[200:250])

p8 <- predict(UBCF_C_P, book_matrix[200:250])

p9 <- predict(UBCF_Z_P, book_matrix[200:250])

boxplot(as.vector(as(p9, "matrix")), col = "green", main = "Distribution of Predicted Values for UBCF Z-Score/Cosine Model", ylab = "Ratings")


hist(as.vector(as(p9, "matrix")), main = "Distrib. of Predicted Values for UBCF Z-Score/Cosine Model", col = "green", xlab = "Predicted Ratings")



#Inferences:
#A direct comparison of the summary statistics for the raw data and the predictions 
#obtained from the UBCF_Z_C model shows that the predicted values appear to fall 
#in the first quarter more as compared to 3rd quartile.

#As shown above, Z-score normalization outperformed centering-based 
#normalization, and both of those normalization approaches outperformed a model 
#constructed using non-normalized data. 
#However, these models do not outperform the Euclidean Distance-based models, and 
#their performance relative to the cosine similarity-based models appears mixed.

#As such, the Euclidean Distance should be preferred over cosine similarity when 
#developing a user-based collaborative filtering recommender for our data set.


