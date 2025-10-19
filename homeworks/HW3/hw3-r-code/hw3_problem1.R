library(Stat2Data)
data("Fluorescence")

# Saving X and Y as variables for easiness
X = Fluorescence$Calcium
Y = Fluorescence$ProteinProp
# Part a : Report the correlations between two variables
round(cor(X,Y), 3)

# Part b : Make a scatter plot with calcium as X and ProteinProp as Y
plot(x=X,
     y=Y,
     main="Scatter Plot of Calcium Concentration v. Proportion of Protein Bound",
     xlab="Log of the Free Calcium Concentration",
     ylab="Proportion of Protein Bound to Calcium",
     pch=19,
     cex=1,
     col="red",)

# Part c : Fit an SLR 
fit = lm(Y~X)
summary(fit)
abline(fit, col="blue",lwd=4)

# Part g : Plot the residuals
plot(X,
     fit$residuals,
     pch=19,
     cex=2,
     xlab="Fitted Proportion Values",
     ylab="Residuals",
     main="Residual Plot",)