Corolla <- read.csv("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\ToyotaCorolla.csv")
Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(Corolla)

summary(Corolla)

pairs(Corolla)


cor(Corolla)

model.corolla <- lm(Price~ ., data = Corolla)
summary(model.corolla)

#cc and doors are insignificant

model.corollaC <- lm(Price~cc)
summary(model.corollaC)

model.corollaD <- lm(Price~ Doors)
summary(model.corollaD)

model.corollaCD <- lm(Price~ cc +Doors)
summary(model.corollaCD)


#######Scatter plot matrix with Correlations inserted in graph

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

#Partial Correlation Matrix

library(corpcor)
cor2pcor(cor(Corolla))

#diagostic plots

install.packages("car")
library(car)
plot(model.corolla)
qqPlot(model.corolla)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.corolla)
influenceIndexPlot(model.corolla, id.n=3) # Index Plots of the influence measures
influencePlot(model.corolla, id.n=3) # A user friendly representation of the abov

### Regression after deleting the 81st observation
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)

### Variance Inflation Factors
vif(model.corolla)  # VIF is > 10 => collinearity 

#Added-Variable-Plots#
avPlots(model.corolla, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(model.corolla)

plot(model.corolla)

model.final <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight)
summary(model.final)

model.final1 <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = Corolla[-81,])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

plot(model.final1)
qqPlot(model.final1)

#Deletion diagnostic for identifying influential variable
influence.measures(model.final1)
influenceIndexPlot(model.final1, id.n=3) # Index Plots of the influence measures
influencePlot(model.final2, id.n=3)

#removing 961.
model.final2 <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = Corolla[-222,])
summary(model.final2)

finalModel <- lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = Corolla[-961,])
summary(finalModel)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.

