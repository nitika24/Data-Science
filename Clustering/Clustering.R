crime <- read.csv(file.choose())

View(crime)

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(crime[,2:5]) #excluding the crimeversity name columnbefore normalizing
?dist
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
??hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 5 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime, membership)

View(final)


write.csv(final, file="final.csv",row.names = F)

aggregate(crime[,-1],by=list(final$membership),mean)



########################KMeans Clustering ######################################


library(readxl)
mydata <- read.csv(file.choose())
View(mydata)
normalized_data <- scale(mydata[,2:5])
fit <- kmeans(normalized_data, 4) # 4 cluster solution
str(fit)
final2<- data.frame(mydata, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(mydata[,2:5], by=list(fit$cluster), FUN=mean)

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 1:8) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, twss, type="b", xlab="Number of Clusters", ylab="total Within groups sum of squares")   # Look for an "elbow" in the scree plot #

title(sub = "K-Means Clustering Scree-Plot")


library(cluster)

# Using Clara function(Clustering for Large Applications) to find cluster
xcl <- clara(normalized_data,5)
clusplot(xcl)

#using Partition Arround Medoids to find cluster
xpm <- pam(normalized_data,5) 
clusplot(xpm)


#Inferences using hierarichal clustering:
#As per summary we can say group 2 have the higher rate of crime.

