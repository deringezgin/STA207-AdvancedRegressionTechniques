# Getting the Boston Data set

# install.packages("MASS")
library(MASS)
data(Boston)

PCH = 9
CEX = 0.5
COL = "black"

X = Boston$lstat
Y = Boston$medv

# Scattorplot of X v. Y
plot(x=X,
     y=Y, 
     pch=PCH,
     cex=CEX,
     xlab="Lower Status of the Population (percent)",
     ylab="Median Price of Owner-Occupied Homes (in $1000s)",
     main="Scatter Plot of Population Percentage vs. Median Home Prices",)

# Regress medv (Y) on lstat (X) and report the fitted model.
fit = lm(Y ~ X)
summary(fit)
abline(fit, col="RED", lwd=5)

# Identify high leverage points, if any.
h_values = hatvalues(fit)
high_levarage_points = which(h_values > 2 * mean(h_values))
high_levarage_points = unname(high_levarage_points)
high_levarage_count = length(high_levarage_points)
high_levarage_points

# Identify outliers, if any.
outliers = which(rstudent(fit) > 2 | rstudent(fit) < -2)
outliers = unname(outliers)
outlier_count = length(outliers)
outliers

# Identify influential points, if any. Use Cook’s distance and influence plots.
cook = cooks.distance(fit)
influential_points = which(cook > 4 / length(Y))
influential_points = unname(influential_points)
influential_count = length(influential_points)
library(car)
influencePlot(fit)
influential_points

plot(fit, pch=19, which=4)
abline(h=4/length(Y), col="red", lwd=5)