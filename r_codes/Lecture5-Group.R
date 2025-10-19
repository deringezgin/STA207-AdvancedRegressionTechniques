##Lecture-5 Group work
library(UsingR)
fit <- lm(galton$child~galton$parent)
summary(fit)

####
par(mfrow=c(2,2))
plot(x=galton$parent, y=galton$child,main="(a)",xlab="midparent height",ylab="Child's height")
plot(fit$fitted.values,y=galton$child,xlab="(Fitted Values)",ylab="Y values",main="(b)", pch=19)
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
plot(galton$parent,fit$residuals,pch=19,xlab="Mid-parent height",ylab="Residuals",main="(d)")
abline(h=mean(fit$residuals),col="blue",lwd=2)
dev.off()
#BP test
#install.packages("lmtest")
library(lmtest)
bptest(fit) 
#index plot of residuals
plot(fit$residuals,pch=19,ylab="Residuals",main="Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")
