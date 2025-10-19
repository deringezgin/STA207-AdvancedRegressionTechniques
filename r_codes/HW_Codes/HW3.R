###HW-3
library(Stat2Data)
data("Fluorescence")
names(Fluorescence) #check variable names

X = Fluorescence$Calcium
Y = Fluorescence$ProteinProp

cor(X,Y)
plot(X,Y,pch=19,xlab="Log of free calcium concentration  ",ylab="Proportion of protein bound to calcium",col="hotpink",main="Figure 1")
model = lm(Y~X)
summary(model)
plot(X,Y,pch=19,xlab="Log of free calcium concentration  ",ylab="Proportion of protein bound to calcium",col="hotpink",main="Figure 2")
abline(model,lwd=3,col="blue")
#residual plot
plot(X,residuals(model),pch=19,xlab="Log of free calcium concentration  ",ylab="Fitted Proportion of protein bound to calcium",col="hotpink",main="Figure 3")


##Problem:3
temp = c(42,37,46,30,50,43,43,46,46,49)
pass = c(173,149,185,123,201,174, 175, 188, 186,198)
cor(temp,pass)
plot(temp,pass,pch=19,xlab="Temperature (F)",ylab="Number of passengers",col="blue",main="Figure 1")
model = lm(pass~temp)
summary(model)
model$residuals
model$fitted.values
#Problem:4
data("TextPrices")
cor(TextPrices$Pages,TextPrices$Price)
plot(TextPrices$Pages,TextPrices$Price,main="Fig. 1: Textbook Prices",ylab="Price",xlab="Pages",pch=19,col="blue")
fit3 <- lm(TextPrices$Price~TextPrices$Pages)
summary(fit3)
plot(TextPrices$Pages,TextPrices$Price,main="Fig. 2: Textbook Prices",ylab="Price",xlab="Pages",pch=19,col="blue")
abline(fit3,col="orange",lwd=3)
plot(fit3$fitted.values,fit3$residuals,main="Fig. 3: Residual Plot",xlab="Fitted Prices",ylab="Residuals",col="red",pch=17)
abline(h=0,lwd=3)
