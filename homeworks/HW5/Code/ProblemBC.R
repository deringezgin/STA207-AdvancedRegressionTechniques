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

### PROBLEM-B ##########################################################################################################

# Scattorplot of X v. Y
plot(x=X,
     y=Y, 
     pch=PCH,
     cex=CEX,
     xlab="Lower Status of the Population (percent)",
     ylab="Median Price of Owner-Occupied Homes (in $1000s)",
     main="Scatter Plot of Population Percentage vs. Median Home Prices",)


# The best fit line
fit = lm(Y ~ X)
summary(fit)
abline(fit, col="RED", lwd=5)


# Residual Plot
plot(x=X,
     y=fit$residuals,
     pch=PCH,
     cex=CEX,
     xlab="Lower Status of the Population (percent)",
     ylab="Residuals",
     main="Residual Plot for medv – lstat",
     col=COL)

