# 1. Install the package and get it ready for this session.
# install.packages("UsingR")

# 2. Open help file for the data and use it to understand variables.
library(UsingR)
data("Galton")
# ?Galton

# 3. What is the goal of this study? State the response and predictor variables.
# This data set is used by Galton in 1886 to study the relationship between a parent's height and their childrens.
# The predictor variable is the parent height while the response variable is the child height.
X = Galton$parent
Y = Galton$child
CEX = 0.8
PCH = 19

# 4.Check the data summaries and make a plot for each variable.
# How many missing values we have? do you see any outliers? anything unusual?
summary(X)
summary(Y)

hist(X,
     xlab="Mid-Parent Height",
     ylab="Frequency",
     main="Histogram of Mid-Parent Height",)

hist(Y,
     xlab="Child Height",
     ylab="Frequency",
     main="Histogram of Child Height",)

sum(is.na(galton)) # Sum of the missing values.
# There are no missing values in the data set.

# 5. Using correlation coefficient and scatter plot, comment on the relationship.
round(cor(X, Y), 3) # --> 0.459

plot(x=X,
     y=Y,
     xlab="Mid-Parent Height (in inches)",
     ylab="Child Height (in inches)",
     main="Scatter Plot of Mid-Parents Height v. Child Height",
     pch=PCH,
     cex=CEX,)

# 6. Fit a linear regression model and show the fitted model.
fit = lm(Y ~ X)

# 7. Interpret the regression coefficients (slope and intercept) of the model?
summary(fit)
abline(fit, col="blue", lwd=4)

# Regresssion Coefficients
# Slope ==> 0.646
# Intercept ==> 23.942


# 8. Check goodness of fit of this model?
# Multiple R-Squared ==> 0.2105
# Residual Standard Error ==> 2.239
# Residual Plot
plot(x=X,
     y=fit$residuals,
     xlab="Fitted values",
     ylab="Residuals",
     main="Residual Plot",
     pch=PCH,
     cex=CEX)

# 9. Show fitted values in the scatterplot.


# 10. Are regression coefficients statistically significant at 5% significance level?
# Yes, they are statistically significant at 5% significance level considering there is 
# *** in the summary of the model.

# 11. Report 99% Cis for regression parameters.
confint(fit, level=0.99)

par(mfrow=c(2,2))

# Scatterplot of Y versus X in SLR
plot(X,
     Y,
     pch=19,
     cex=0.8,
     col="blue",
     main="Scatterplot of Y versus X | Plot (A)")

# Scatterplot of Predicted Y versus Y
plot(x=fit$fitted.values,
     y=Y,
     xlab="Fitted Values",
     ylab="Real Values",
     main="Fitted Values v. Real Values | Plot (B)",
     pch=PCH,
     cex=CEX)

# Residual Plot
plot(x=X,
     y=fit$residuals,
     xlab="Fitted values",
     ylab="Residuals",
     main="Residual Plot | Plot (C)",
     pch=PCH,
     cex=CEX)
installed.packages("car")
library(car)
Tukey.test = residualPlot(fit,
                          pch=17,
                          main="Tukey's Test Plot | Plot (D)")
Tukey.test

par(mfrow=c(2,2))
hist(fit$residuals,main="Histogram of Residuals | Plot (A)",xlab ="Residuals")
qqnorm(fit$residuals,pch=19, main="Normal Q-Q Plot | Plot (B)")
plot(fit$residuals,pch=19,ylab="Residuals",main="Residuals | Plot (C)")
##Residual Plot
plot(X,fit$residuals,pch=19,xlab="Temperature",ylab="Residuals",main="Residual Plot | Plot (D)")
abline(h=mean(fit$residuals),col="blue",lwd=2)


library(lmtest) #we have to install this package
bptest(fit)
