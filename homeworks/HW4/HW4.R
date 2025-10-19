library(Stat2Data)
library(car)
data("TextPrices")

X = TextPrices$Pages
Y = TextPrices$Price
PCH = 19
CEX = 1

# Problem 1.1 Getting the fit line
fit = lm(Y~X)
summary(fit)

# Problem 1.2 Determining the confidence intervals
# 95%
confint(fit, level=0.95)

# 90%
confint(fit, level=0.9)


##### REPORTING LINE #####
# Problem 1.3

par(mfrow=c(2,2))

plot(x=X,
     y=Y,
     main="Number of Pages [X] v. Textbook's Price [Y] (Plot-A)",
     ylab="Textbook Price",
     xlab="Page Count",
     pch=PCH,
     col="blue")

plot(x=fit$fitted.values,
     y=Y,
     main="Fitted Price v. Real Price (Plot-B)",
     ylab="Price",
     xlab="Fitted Price",
     pch=PCH,
     col="blue")

plot(x=fit$fitted.values,
     y=fit$residuals,
     main="Residual Plot (Plot-C)",
     xlab="Fitted Prices",
     ylab="Residuals",
     col="red",
     pch=PCH)
abline(h=0,lwd=2)

Tukey.test <- residualPlot(fit,pch=PCH,main="Tukey's Curve Test (Plot-D)")
Tukey.test
dev.off()

### Independence of Errors ###
plot(fit$residuals,
     pch=PCH,
     ylab="Residuals",
     main="Index Plot of Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")

### Normality of Errors ###
par(mfrow=c(1,2))
hist(fit$residuals,main="Histogram of Residuals",xlab = "Residuals")
qqnorm(fit$residuals,pch=PCH)
dev.off()

### Errors are Homoscedastic ###
plot(x=X,
     y=fit$residuals,
     main="Residual Plot",
     xlab="Page Count",
     ylab="Residuals",
     col="red",
     pch=PCH)
abline(h=0,lwd=2)

# Breusch-Pagan Test
library(lmtest)
bptest(fit)

