library(MASS)
library(lmtest)
library(car)
data(Boston)
############
#Problem1:
#############
#scatter plot of X vs Y
plot(medv ~ lstat, data = Boston, xlab="% in Poverty", ylab="Median Home Prices", main="Fig1: Home Prices and Poverty around Boston", pch=16, col="peru")
#linear model
model1 <- lm(medv ~ lstat, data = Boston)
summary(model1)

hvalues = hatvalues(model1)
hvaluesthresh = 2*mean(hatvalues(model1))
index.leverage = which(hvalues>hvaluesthresh)
unname(index.leverage)
length(index.leverage)

z = rstandard(model1)
index.resid <- which(abs(z)>2)
unname(index.resid)
length(index.resid)

cd <- cooks.distance(model1)
index.cd <- which(cd>4/nrow(Boston))
unname(index.cd)
length(index.cd)
plot(model1, pch=16, which=4,main="Fig.1")#Cook's distance
abline(h=4/nrow(Boston),col="red",lwd=3)
influencePlot(model1,main="Fig.2")

##Problem:2
#install.packages("ISLR")
library(ISLR)
names(Credit)
plot(Credit[,c(12,3,2,4)], pch=16, col="peru", main="Fig.3: Matrix Scatterplot")
credit_cor <- cor(Credit[,c(12,3,2,4)])
dev.off()
library(corrplot)
corrplot(credit_cor, method = "number",title="Fig.4: Correlation Plot",mar = c(0, 0, 1, 0))
#Run regresison and interpret
debt_model <- lm(Balance~ Limit + Income+Rating, data = Credit)
summary(debt_model)
plot(debt_model,which=1,main="Fig.5:Residual Plot")
#LINE condiitons
#scatterplot of Y vs Yhat
par(mfrow=c(3,2))
plot(debt_model$fitted.values,Credit$Balance,pch=19,main="Fig.6(a):Scatterplot of Y vs Fitted Y",xlab="Fitted Debt ($)",ylab="Debt ($)")
library(car)
Tukey.test <- residualPlot(debt_model,pch=17,main="Fig6(b):Tukey's Curve")
Tukey.test
hist(debt_model$residuals,main="Fig.6(c):Histogram of Residuals",xlab="Residuals")
qqnorm(debt_model$residuals,pch=19,main="Fig.6(d):QQPlot")
plot(debt_model,which=1,main="Fig.6(e):Residual Plot")
plot(debt_model$residuals,pch=19,ylab="Residuals",main="Fig.6(f):Residuals Index Plot")
abline(h=mean(debt_model$residuals),lwd=4,col="red")
library(lmtest)
bptest(debt_model)
#
library(car)
vif_model = vif(debt_model)
which(vif_model>4)


