getwd()

cdata <- read.csv ("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\Computer_data.csv")
View(cdata)

attach(cdata)

cdata[1] = NULL
View(cdata)
attach(cdata)


summary(cdata) # Explore the data

 # Plot relation ships between each X with Y
## Or make a combined plot
pairs(cdata)   # Scatter plot for all pairs of variables
#cor(HP,MPG)
 # correlation matrix

# The Linear Model of interest
model.cdata <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend, data=cdata)
#model.car = lm(MPG ~ . , data = cdata)
# lm(Y ~ X)
summary(model.cdata)


#######                    Scatter plot matrix with Correlations inserted in graph


###                                   Partial Correlation matrix
install.packages("car")
library(car)


??avPlots
avPlots(model.cdata, id.n=2, id.cex=0.8, col="red")

vif(model.cdata)

#influence.measures(model.cdata)
#influenceIndexPlot(model.cdata)
#influencePlot(model.cdata)

model.cdata2 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend, data=cdata[ -c(1441,1701),])
summary(model.cdata2)

vif(model.cdata2)
avPlots(model.cdata2)

plot(model.cdata)

qqPlot(model.cdata)
