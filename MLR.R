getwd()

Startups <- read.csv ("D:\\Nitika\\Data Science\\DataSets\\Assignments_DataSets\\50_Startups.csv")
View(Startups)

attach(Startups)

Startups[4] = NULL
View(Startups)
attach(Startups)


summary(Startups) # Explore the data

pairs(Startups)   # Scatter plot for all pairs of variables
#cor(HP,MPG)
cor(Startups) # correlation matrix

# The Linear Model of interest
model.Startup <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
#model.car = lm(MPG ~ . , data = Startups)
# lm(Y ~ X)
summary(model.Startup)

#Marketing.Spend and Administration are insignificant

model.StartupM <- lm(Profit ~ Marketing.Spend)
summary(model.StartupM)

model.StartupA <- lm(Profit~ Administration)
summary(model.StartupA)

model.StartupAM <- lm(Profit ~ Administration+Marketing.Spend)
summary(model.StartupAM)

#######                    Scatter plot matrix with Correlations inserted in graph
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
pairs(Startups, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")



###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(Startups)

cor2pcor(cor(Startups))
?cor2pcor

install.packages("car")
library(car)

#Diagnostic plots

plot(model.Startup)

qqPlot(model.Startup)

# Deletion Diagnostics for identifying influential variable
influence.measures(model.Startup)
influenceIndexPlot(model.Startup, id.n=3) # Index Plots of the influence measures
influencePlot(model.Startup, id.n=3) # A user friendly representation of the above


## Regression after deleting the 50th observation
model.Startup1<-lm(Profit~. , data=Startups[-50])
summary(model.Startup1)


### Variance Inflation Factors
vif(model.Startup)  # VIF is > 10 => collinearity

#Added-Variable-Plots#
avPlots(model.Startup, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(model.Startup)

plot(model.Startup)

model.final <- lm(Profit ~ R.D.Spend + Marketing.Spend)
summary(model.final)

model.final1 <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = Startups[-50,])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.