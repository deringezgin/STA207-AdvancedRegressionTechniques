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

### PROBLEM-A ##########################################################################################################

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

h_values = hatvalues(fit)
index.levarage = which(hatvalues(fit)>(2*mean(hatvalues(fit))))
length(index.levarage)

rstandard_vals = rstandard(fit)
index.outliers = which(abs(rstandard_vals) >2)
length(index.outliers)

cooks_vals = cooks.distance(fit)

plot(fit, pch=16, which=1)
plot(fit, pch=16, which=4)
abline(a=4/506, b=0, col="red")
index.cook = which(abs(cooks_vals) > (4 /506))
length(index.cook)

library(car)
influencePlot(fit)









