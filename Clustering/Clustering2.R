#####################HClustering#################################

install.packages("xlsx")
library(xlsx)

lines <- read.xlsx("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\EastWestAirlines.xlsx", sheetIndex = 2)

View(lines)

# Normalizing continuous columns to bring them under same scale
normalized_data1<-scale(lines[,2:12]) #excluding the linesversity name columnbefore normalizing
?dist
View(normalized_data1)
d <- dist(normalized_data1, method = "euclidean") # distance matrix

??hclust
fit <- hclust(d, method="complete")
?hclust

plot(fit) # display dendrogram
plot(fit, hang=-1)


?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 5 clusters

table(groups)

g1 = aggregate(normalized_data1,list(groups),median)
data.frame(Cluster=g1[,1],Freq=as.vector(table(groups)),g1[,-1])

membership<-as.matrix(groups) # groups or cluster numbers
final1 <- data.frame(lines, membership)



####################KMeans Clustering######################################3

library(readxl)
mydata <- read.xlsx("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\EastWestAirlines.xlsx", sheetIndex = 2)
View(mydata)
normalized_data <- scale(mydata[,2:12])
fit <- kmeans(normalized_data, 8) # 4 cluster solution
str(fit)
final2<- data.frame(mydata, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(mydata[,2:12], by=list(fit$cluster), FUN=mean)

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 1:32) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:32, twss, type="b", xlab="Number of Clusters", ylab="total Within groups sum of squares")   # Look for an "elbow" in the scree plot #

title(sub = "K-Means Clustering Scree-Plot")


# Inferences:
#We can see that group 1 has the highest number of similar charactersitcs.

