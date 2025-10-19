###MLR code####
###Lecture8####
#read autompg data
autompg <- read.csv("autompg.csv")
names(autompg)
#selecting certain columns/variables from the data
plot(autompg[,c(2,5,6)], pch=16, col="green", main="Matrix Scatterplot")
autompg_cor <- cor(autompg[,c(2,5,6)])
#need to get library
#install.packages(corrplot)
library(corrplot)
corrplot(autompg_cor, method = "number")
##extra example: mpg, cyl, disp, and hp
plot(autompg[,c(2,3,4,5)], pch=16, col="green", main="Matrix Scatterplot")
#mpg, et, acc, year
plot(autompg[,c(2,6,7,8)], pch=16, col="green", main="Matrix Scatterplot")
autompg_cor_2 <- cor(autompg[,c(2,6,7,8)])
corrplot(autompg_cor_2, method = "number")

##new one
plot(autompg[,c(2,4,6,3,5,7,8)], pch=16, col="green", main="Matrix Scatterplot")
autompg_cor <- cor(autompg[,c(2,4,6,3,5,7,8)])
corrplot(autompg_cor, method = "number")
dev.off()

#MLR model
mpg_model <- lm(mpg ~ wt+hp, data = autompg)
summary(mpg_model)
coef(mpg_model)
##Residual plot: way1
plot(mpg_model$fitted.values,mpg_model$residuals,main="Residual Plot",xlab="Firtted mpg",ylab="Residual",pch=19,col="blue")
#easier way
plot(mpg_model,which=1,main="Residual Plot")
###LINE with six plots
par(mfrow=c(3,2))
#Plot1: Y vs Yhat
plot(mpg_model$fitted.values,autompg$mpg,pch=19,main="Fig.1(a):Scatterplot of Y vs Fitted Y",xlab="Fitted Debt ($)",ylab="Debt ($)")
library(car)
Tukey.test <- residualPlot(mpg_model,pch=17,main="Fig2(b):Tukey's Curve")
Tukey.test
hist(mpg_model$residuals,main="Fig.2(c):Histogram of Residuals",xlab="Residuals")
plot(mpg_model,which=2,main="Fig.2(d):QQPlot")
plot(mpg_model,which=1,main="Fig.2(e):Residual Plot")
plot(mpg_model$residuals,pch=19,ylab="Residuals",main="Fig.2(f):Residuals Index Plot")
abline(h=mean(mpg_model$residuals),lwd=4,col="red")
library(lmtest)
bptest(mpg_model)

##VIF
#install.packages ("car", dep=T) 
library(car)
vif(mpg_model) # variance inflation factors 
vif_model = vif(mpg_model)
vif_model
which(round(vif_model)>4)
which(round(vif_model)==4)
which(round(vif_model)>=4)

######################