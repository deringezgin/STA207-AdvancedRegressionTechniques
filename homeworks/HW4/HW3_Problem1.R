library(Stat2Data)
data("TextPrices")
X = TextPrices$Pages
Y = TextPrices$Price
PCH = 19
CEX = 1

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

confint(fit,level=0.99)

confint(fit, level=0.95)

confint(fit, level=0.9)


plot(X,
     fit$residuals,
     pch=19,
     cex=2,
     xlab="Fitted Price",
     ylab="Residuals",
     main="Residual Plot",)
fit$residuals

par(mfrow=c(2,2))

# Scatterplot of Y versus X in SLR
plot(X, Y, pch=PCH, cex=CEX, col="blue", main="Scatterplot of Y versus X")

# Scatterplot of Predicted Y versus Y
plot(x=fit$fitted.values,
     y=Y,
     xlab="Fitted Values",
     ylab="Real Values",
     main="Fitted Values v. Real Values",
     pch=PCH,
     cex=CEX)

# Residual Plot
plot(x=X,
     y=fit$residuals,
     xlab="Fitted values",
     ylab="Residuals",
     main="Residual Plot",
     pch=PCH,
     cex=CEX)
# install.packages("car")
# library(car)
Tukey.test = residualPlot(fit,
                          pch=PCH)
Tukey.test


