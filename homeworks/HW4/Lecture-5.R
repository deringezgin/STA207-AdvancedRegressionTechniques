###Data from class
##SLR R-code
##X: temp in F and Y: viscosity in centipoise
X <- c(25,35,45,55,65,75,85,95)
Y <- c(1,0.98,0.85,0.78,0.67,0.64,0.55,0.45)

##regression model: Y regressed on X
fit <- lm(Y~X)
summary(fit)

par(mfrow=c(2,2))
##plot
plot(X,Y,ylab="Viscosity",xlab="Temperature",pch=19,cex=2,main="Temperature vs Viscosity")
plot(fit$fitted.values,Y,ylab="Estimated Viscosity",xlab="Viscosity",pch=19,cex=2,main="Fitted vs Observed")
##Residual Plot
plot(X,fit$residuals,pch=19,cex=2,xlab="Temperature",ylab="Residuals",main="Residual Plot")
dev.off()
install.packages("car")
library(car)
Tukey.test <- residualPlot(fit,pch=17)
Tukey.test

###Assumptions on Error
#histogram
par(mfrow=c(2,2))
hist(fit$residuals,main="Histogram of Residuals",xlab = "Residuals")
qqnorm(fit$residuals,pch=19)
plot(fit$residuals,pch=19,ylab="Residuals",main="Residuals")
##Residual Plot
plot(X,fit$residuals,pch=19,xlab="Temperature",ylab="Residuals",main="Residual Plot")
abline(h=mean(fit$residuals),col="blue",lwd=2)
dev.off()
#BP test
install.packages("lmtest")
library(lmtest)
bptest(fit) 
#index plot of residuals
plot(fit$residuals,pch=19,ylab="Residuals",main="Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")
