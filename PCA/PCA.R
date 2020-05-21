# Loading Universities data
install.packages("NbClust")
install.packages("factoextra")
library(tw)
install.packages("fpc")
library(factoextra)
library(fpc)
library(NbClust)
install.packages("xlsx")
library(xlsx)
mydata = read.csv("D:\\Nitika\\Data Science\\Assignments\\PCA\\wine.csv")


View(mydata)


help(princomp) ## to understand the api for princomp




# mydata[-1] -> Considering only numerical values for applying PCA
data <- mydata[-1]
View(data)
attach(data)
cor(mydata)
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)



#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]

#Cluster Analysis - All Variables

no_of_Clusters = NbClust(clus_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")


# Plot bar chart for the clusters
fviz_nbclust(no_of_Clusters) + theme_minimal()



# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage
plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,4) # Cutting the dendrogram for 5 clusters

#K-Means Clustering
fit2 <- kmeans(dist1, 4) # 4 cluster solution
str(fit2)
final2<- data.frame(clus_data, fit2$cluster) # append cluster membership
final2
View(clus_data)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(clus_data, by=list(fit2$cluster), FUN=mean)

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(clus_data)-1)*sum(apply(clus_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 1:4) twss[i] = sum(kmeans(clus_data, centers=i)$withinss)
plot(1:4, twss, type="b", xlab="Number of Clusters", ylab="total Within groups sum of squares")   # Look for an "elbow" in the scree plot #

#K-Means Clustering - PCA suggested components
km.7 = eclust(dist1, "kmeans", k = 4, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")

title(sub = "K-Means Clustering Scree-Plot")



numbers<-as.matrix(groups) # cluster numbering 

View(numbers)

final1<-cbind(numbers,mydata) # binding column wise with orginal data
View(final1)

write.csv(final1,file="wine.csv",row.names = F,col.names = F)
getwd()

#Number of clusters on original data
#Cluster Analysis - All Variables

no_of_Clusters = NbClust(data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")


# Plot bar chart for the clusters
fviz_nbclust(no_of_Clusters) + theme_minimal()


#Inferences:
#Number of clusters are different using PCA data and original data are different.
#PCA data has 4 optimum clusters whereas original data has 7 optimum clusters.