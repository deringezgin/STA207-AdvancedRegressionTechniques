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
####residual plot of linear model
plot(model1$fitted.values, model1$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig2: Residual Plot", pch=16, col="peru")

###LINE for linear model
####
par(mfrow=c(2,2))
plot(medv ~ lstat, data = Boston, xlab="% in Poverty", 
     ylab="Median Home Prices", main="Fig.3(a)", pch=16, col="peru")
plot(model1$fitted.values,Boston$medv,xlab="Fitted Median Home Values ($)",ylab="Actual Mediam Home Price ($)",main="Fig.3(b)", pch=19,col="peru")
plot(model1$fitted.values,model1$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.3(c)", pch=19,col="peru")
Tukey.test <- residualPlot(model1,pch=17,main="Fig.3(d)",col="peru")
Tukey.test
#histogram
par(mfrow=c(2,2))
hist(model1$residuals,main="Fig.4(a)",xlab = "Residuals",col="peru")
qqnorm(model1$residuals,pch=19,main="Fig.4(b)",col="peru")
plot(model1$residuals,pch=19,ylab="Residuals",main="Fig.4(c)",col="peru")
bptest(model1) 

#############################
#Log transformations ########
###Three transformed models##
#log-log
model2 <- lm(log(medv)~log(lstat),data=Boston)
summary(model2)
#logY
model3 <- lm(log(medv)~lstat,data=Boston)
summary(model3)
#logX
model4 <- lm(medv~log(lstat),data=Boston)
summary(model4)
#Residual Plots for three transformed models
par(mfrow=c(2,2))
plot(model2$fitted.values,model2$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.5(a)", pch=19,col="peru")
plot(model3$fitted.values,model3$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.5(b)", pch=19,col="peru")
plot(model4$fitted.values,model4$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.5(c)", pch=19,col="peru")
#####LINE ####
###LINE for log-log
par(mfrow=c(2,2))
plot(log(medv) ~ log(lstat), data = Boston, xlab="log % in Poverty", 
     ylab="log Median Home Prices", main="Fig.6(a)", pch=16, col="peru")
plot(model2$fitted.values,log(Boston$medv),xlab="Fitted log Median Home Values ($)",ylab="Actual Mediam Home Price ($)",main="Fig.6(b)", pch=19,col="peru")
plot(model2$fitted.values,model2$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.6(c)", pch=19,col="peru")
Tukey.test <- residualPlot(model2,pch=17,main="Fig.6(d)",col="peru")
Tukey.test
#histogram
par(mfrow=c(2,2))
hist(model2$residuals,main="Fig.7(a)",xlab = "Residuals",col="peru")
qqnorm(model2$residuals,pch=19,main="Fig.7(b)",col="peru")
plot(model2$residuals,pch=19,ylab="Residuals",main="Fig.7(c)",col="peru")
##Residual Plot
plot(model2$fitted.values, model3$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig.7(d)", pch=16, col="peru")
abline(h=mean(model2$residuals),col="blue",lwd=2)

bptest(model2) 
#####LINE ####
#log Y
###############
par(mfrow=c(2,2))
plot(log(medv) ~ lstat, data = Boston, xlab="% in Poverty", 
     ylab="log Median Home Prices", main="Fig.8(a)", pch=16, col="peru")
plot(model3$fitted.values,log(Boston$medv),xlab="Fitted log Median Home Values ($)",ylab="Actual Mediam Home Price ($)",main="Fig.8(b)", pch=19,col="peru")
plot(model3$fitted.values,model3$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.8(c)", pch=19,col="peru")
Tukey.test <- residualPlot(model3,pch=17,main="Fig.8(d)",col="peru")
Tukey.test
#histogram
par(mfrow=c(2,2))
hist(model3$residuals,main="Fig.9(a)",xlab = "Residuals",col="peru")
qqnorm(model3$residuals,pch=19,main="Fig.9(b)",col="peru")
plot(model3$residuals,pch=19,ylab="Residuals",main="Fig.9(c)",col="peru")
##Residual Plot
plot(model3$fitted.values, model3$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig.9(d)", pch=16, col="peru")
abline(h=mean(model3$residuals),col="blue",lwd=2)
bptest(model3) 


#####LINE ####
#log(X)
###############
####
par(mfrow=c(2,2))
plot(medv ~ log(lstat), data = Boston, xlab="log % in Poverty", 
     ylab="Median Home Prices", main="Fig.10(a)", pch=16, col="peru")
plot(model4$fitted.values,Boston$medv,xlab="Fitted Median Home Values ($)",ylab="Actual Mediam Home Price ($)",main="Fig.10(b)", pch=19,col="peru")
plot(model4$fitted.values,model4$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.10(c)", pch=19,col="peru")
Tukey.test <- residualPlot(model4,pch=17,main="Fig.10(d)",col="peru")
Tukey.test
#histogram
par(mfrow=c(2,2))
hist(model4$residuals,main="Fig.11(a)",xlab = "Residuals",col="peru")
qqnorm(model4$residuals,pch=19,main="Fig.11(b)",col="peru")
plot(model4$residuals,pch=19,ylab="Residuals",main="Fig.11(c)",col="peru")
##Residual Plot
plot(model4$fitted.values, model4$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig.11(d)", pch=16, col="peru")
abline(h=mean(model4$residuals),col="blue",lwd=2)
bptest(model4) 

#Prob-2#
#############
#Polynomial
#############
model5 <- lm(medv ~ lstat+I(lstat ^ 2), data = Boston)
summary(model5)
plot(model5$fitted.values, model5$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig12: Residual Plot", pch=16, col="peru")
####LINE #
par(mfrow=c(2,2))
x.poly =(Boston$lstat)^2
plot(medv ~x.poly, data = Boston, xlab="% in Poverty quadratic polynomial", 
     ylab="Median Home Prices", main="Fig.13(a)", pch=16, col="peru")
plot(model5$fitted.values,Boston$medv,xlab="Fitted Median Home Values ($)",ylab="Actual Mediam Home Price ($)",main="Fig.13(b)", pch=19,col="peru")
plot(model5$fitted.values,model1$residuals,xlab="(Fitted values)",ylab="Residuals",main="Fig.13(c)", pch=19,col="peru")
Tukey.test <- residualPlot(model5,pch=17,main="Fig.13(d)",col="peru")
Tukey.test

#histogram
par(mfrow=c(2,2))
hist(model5$residuals,main="Fig.14(a)",xlab = "Residuals",col="peru")
qqnorm(model5$residuals,pch=19,main="Fig.14(b)",col="peru")
plot(model5$residuals,pch=19,ylab="Residuals",main="Fig.14(c)",col="peru")
##Residual Plot
plot(model5$fitted.values, model5$residuals,
     xlab="Y fitted values", ylab="Residuals", 
     main="Fig.14(d)", pch=16, col="peru")
abline(h=mean(model5$residuals),col="blue",lwd=2)
bptest(model5) 
dev.off()
