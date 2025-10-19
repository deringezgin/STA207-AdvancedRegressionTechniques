# install.packages("MASS")
library(MASS)
data(Boston)
help(Boston)

PCH = 19
CEX = 0.5
COL = "black"

X = Boston$lstat
Y = Boston$medv
########################################################################################################################
X = log(X)
### PROBLEM-FC #########################################################################################################

fit = lm(Y ~ X)
summary(fit)

plot(x=X,
     y=fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Log of Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for medv – log(lstat)",
     col=COL)

############################################################################################
########## Checking the Line Assumptions #########
# L: There is a linear relationship between X and Y variables

par(mfrow=c(2,2))

# Scatterplot of Y vs. X
plot(x=X,
     y=Y,
     xlab="Log of Lower Status of the Population (percent)",
     ylab="Median Value of Owner-Occupied Homes (in $1000s)",
     pch=PCH,
     cex=CEX,
     main="Scatterplot of Log of Population Percentage [X] vs. Median Home Prices [Y] (Plot-A)")

# Scatterplot of Fitted Values vs Real Values
plot(x=Y,
     y=fit$fitted.values,
     xlab="Median Value of Owner-Occupied Homes (in $1000s)",
     ylab="Fitted Values",
     pch=PCH,
     cex=CEX,
     main="Scatterplot of Real Median Home Price [X] vs. Fitted Median Home Price [Y] (Plot-B)")

# Residual Plot
plot(x=fit$fitted.values,
     y=fit$residuals,
     xlab="Fitted Prices",
     ylab="Residuals",
     pch=PCH,
     cex=CEX,
     main="Residual Plot (Plot-C)")

# Tukey Test
library(car)
Tukey.test = residualPlot(fit,pch=PCH,main="Tukey's Curve Test (Plot-D)")
Tukey.test


dev.off()


########## I: Errors are independent #########


# Index plot of residuals
plot(fit$residuals,
     pch=PCH,
     ylab="Residuals",
     main="Index Plot of Residuals")
abline(h=mean(fit$residuals),lwd=2,col="blue")

######### N: Errors are normally distributed ##########


par(mfrow=c(1,2))
# Histogram of residuals
hist(fit$residuals,
     main="Histogram of Residuals",
     xlab="Residuals",)

# Normal Q-Q Plot
qqnorm(fit$residuals,
       main="Normal Q-Q Plot",)


dev.off()


# ########## E: Errors are homoscedastic/have equal variance, Var(ϵ) = σ2 ##########
#
#
# Residual plot
plot(x=fit$fitted.values,
     y=fit$residuals,
     xlab="Fitted Prices",
     ylab="Residuals",
     pch=PCH,
     cex=CEX,
     main="Residual Plot")
abline(h=mean(fit$residuals),lwd=2,col="blue")

# BP-Test
library(lmtest)
bptest(fit)





