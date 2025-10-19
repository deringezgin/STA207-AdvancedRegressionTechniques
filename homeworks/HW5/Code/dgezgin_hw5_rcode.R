########## PRE-HOMEWORK INSTALLATION ##################################################################################
# Installing the packages for housing prices in neighborhoods in the suburbs of Boston

# install.packages("MASS")
library(MASS)
data(Boston)
help(Boston)

PCH = 19
CEX = 0.5
COL = "black"

X = Boston$lstat
Y = Boston$medv





########## PROBLEM-F ##################################################################################################


log_X = log(X)
log_Y = log(Y)

# Regress log(medv) on log(lstat) and report the fitted model.
log_Y_log_X_fit = lm(log_Y ~ log_X)
summary(log_Y_log_X_fit)

plot(x=log_X,
     y=log_Y_log_X_fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Log of Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for log(medv) – log(lstat)",
     col=COL)

############################################################################################

# Regress log(medv) on lstat and report the fitted model.
log_Y_X_fit = lm(log_Y ~ X)
summary(log_Y_X_fit)

plot(x=X,
     y=log_Y_X_fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for log(medv) – lstat",
     col=COL)

############################################################################################

# Regress medv on log(lstat) and report the fitted model.
Y_log_X_fit = lm(Y ~ log_X)
summary(Y_log_X_fit)

plot(x=log_X,
     y=Y_log_X_fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Log of Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for medv – log(lstat)",
     col=COL)

############################################################################################

# Regress medv on lstat and lstat squared, that is a polynomial model.
poly_fit = lm(Y ~ X + I(X^2))
summary(poly_fit)

# Residuals
plot(x=X,
     y=poly_fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for medv – lstat and lstat squared",
     col=COL)

############################################################################################

# Checking the Line Assumptions
par(mfrow=c(2,2))

# Scatterplot of Y vs. X
plot(x=X,
     y=Y,
     xlab="Lower Status of the Population (percent)",
     ylab="Median Value of Owner-Occupied Homes (in $1000s)",
     pch=PCH,
     cex=CEX,
     main="Scatterplot of Population Percentage [X] vs. Median Home Prices [Y] (Plot-A)")

# Scatterplot of Fitted Values vs Real Values
plot(x=Y,
     y=poly_fit$fitted.values,
     xlab="Median Value of Owner-Occupied Homes (in $1000s)",
     ylab="Fitted Values",
     pch=PCH,
     cex=CEX,
     main="Scatterplot of Real Median Home Price [X] vs. Fitted Median Home Price [Y] (Plot-B)")

# Residual Plot
plot(x=poly_fit$fitted.values,
     y=poly_fit$residuals,
     xlab="Fitted Prices",
     ylab="Residuals",
     pch=PCH,
     cex=CEX,
     main="Residual Plot (Plot-C)")

# Tukey Test
library(car)
Tukey.test = residualPlot(poly_fit,pch=PCH,main="Tukey's Curve Test on the Polynomial Model (Plot-D)")
Tukey.test


dev.off()

# Independence of Errors, Index plot of residiuals
plot(poly_fit$residuals,
     pch=PCH,
     ylab="Residuals",
     main="Index Plot of Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")


par(mfrow=c(1,2))
# Histogram of residuals
hist(poly_fit$residuals,
     main="Histogram of Residuals",
     xlab="Residuals",)

# Normal Q-Q Plot
qqnorm(poly_fit$residuals,
       main="Normal Q-Q Plot",)

































