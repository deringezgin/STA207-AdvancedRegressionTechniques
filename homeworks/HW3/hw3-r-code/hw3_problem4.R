library(Stat2Data)
data("TextPrices")
X = TextPrices$Pages
Y = TextPrices$Price

plot(x=X,
     y=Y,
     xlab="Number of Pages",
     ylab="Price",
     main="Scatterplot of Number of Pages v. Price",
     pch=19,
     cex=1,
     col="red",)

fit = lm(Y~X)
summary(fit)
abline(fit, col="blue", lwd=4)

plot(X,
     fit$residuals,
     pch=19,
     cex=2,
     xlab="Fitted Price",
     ylab="Residuals",
     main="Residual Plot",)
fit$residuals