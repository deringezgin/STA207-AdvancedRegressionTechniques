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

### PROBLEM-E ##########################################################################################################

fit = lm(Y ~ X)
# Residual plot
plot(x=fit$fitted.values,
     y=fit$residuals,
     xlab="Fitted Prices",
     ylab="Residuals",
     pch=PCH,
     cex=CEX,
     main="Residual Plot")
