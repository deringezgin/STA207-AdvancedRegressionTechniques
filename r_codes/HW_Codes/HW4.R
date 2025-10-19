library(Stat2Data)
library(car)
data("TextPrices")
par(mfrow=c(2,2))
plot(TextPrices$Pages,TextPrices$Price,main="(a)",ylab="Price",xlab="Pages",pch=19,col="blue")
fit <- lm(TextPrices$Price~TextPrices$Pages)
summary(fit)
#CIs
confint(fit) #by default it is 95%
confint(fit,level=0.90) #changing to 90%
#LINE

####
par(mfrow=c(2,2))
plot(x=TextPrices$Pages, y=TextPrices$Price,main="(a)",xlab="Pages",ylab="Price $")
plot(fit$fitted.values,y=TextPrices$Price,xlab="Fitted Price $",ylab="Price $",main="(b)", pch=19)
plot(fit$fitted.values,fit$residuals,xlab="(Fitted values)",ylab="Residuals",main="(c)", pch=19)
library(car)
Tukey.test <- residualPlot(fit,pch=17,main="(d)")
Tukey.test

#histogram
par(mfrow=c(2,2))
hist(fit$residuals,main="(a)",xlab = "Residuals")
qqnorm(fit$residuals,pch=19,main="(b)")
plot(fit$residuals,pch=19,ylab="Residuals",main="(c)")
##Residual Plot
plot(TextPrices$Pages,fit$residuals,pch=19,xlab="Pages",ylab="Residuals",main="(d)")
abline(h=mean(fit$residuals),col="blue",lwd=2)
dev.off()
#BP test
#install.packages("lmtest")
library(lmtest)
bptest(fit) 
#index plot of residuals
plot(fit$residuals,pch=19,ylab="Residuals",main="Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")
