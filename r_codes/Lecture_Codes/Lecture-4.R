temp = c(25,35,45,55,65,75,85,95) #in celcius
viscosity = c(1.00,0.98,0.85,0.78,0.67,0.64,0.55,0.45) #in centipoise

plot(temp,viscosity,xlab="Temperature",ylab="Viscosity",main="",pch=19,col="blue")
fit = lm(viscosity~temp)
summary(fit)
plot(temp,viscosity,ylab="Viscosity",xlab="Temperature",pch=19,cex=2)
abline(fit,col="red",lwd=4)
plot(temp,fit$residuals,pch=19,cex=2,xlab="Temperature",ylab="Residuals",main="Residual Plot")
plot(fit$fitted.values,fit$residuals,pch=19,cex=2,xlab="Fitted Viscosity Values",ylab="Residuals",main="Residual Plot")

