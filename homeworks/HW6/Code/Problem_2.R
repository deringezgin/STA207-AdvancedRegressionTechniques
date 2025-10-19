library(ISLR)
library(corrplot)
Credit = Credit

# Make a scatterplot matrix and explain relationship of outcome variable with all three predictor variables.

balance = Credit$Balance
income = Credit$Income
rating = Credit$Rating
limit = Credit$Limit

pairs(Credit[, c("Balance","Limit","Income","Rating")], 
      pch=19, 
      cex=0.5, 
      col="black", 
      main="Scatterplot Matrix of Credit Data",
      labels = c("Balance \n (Average Credit Card Debt)", "Credit Limit", "Income", "Credit Rating"))

# Make a corrplot and see if your explanation in (a) match with the strength and direction of the correlation coefficient for each relationship. 
credit_cor = cor(Credit[, c("Balance", "Limit", "Income", "Rating")])
# Plotting the correlation matrix with custom labels and adjusted margins
corrplot(credit_cor,
         method = "number",
         main = "Correlation Matrix of Credit Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff

# Getting the fitted model
fit = lm(Balance ~ Income + Rating + Limit, data=Credit)
summary(fit)


# Residual plot
plot(x=fit$fitted.values,
     y=fit$residuals,
     main="Residual Plot",
     xlab="Fitted Values",
     ylab="Residuals")

plot(fit,
     which=1,
     main="Residual Plot",)

# Reporting the LINE assumptions
par(mfrow=c(2,3))
#Plot1: Y vs Yhat
plot(x=fit$fitted.values,
     y=Credit$Balance,
     main="Fig.A: Scatterplot of Y vs Fitted Y",
     xlab="Fitted Debt ($)",
     ylab="Debt ($)")

library(car)
Tukey.test <- residualPlot(fit,
                           pch=17,
                           main="Fig.B: Tukey's Curve")
Tukey.test

hist(fit$residuals,
     main="Fig.C: Histogram of Residuals",
     xlab="Residuals")

plot(fit,
     which=2,
     main="Fig.D: QQPlot")

plot(fit,
     which=1,
     main="Fig.E: Residual Plot")

# Index plot of residuals
plot(fit$residuals,
     pch=19,
     ylab="Residuals",
     main="Fig.F: Residuals Index Plot")
abline(h=mean(fit$residuals),lwd=4,col="red")

library(lmtest)
bptest(fit)

### VIF ###
#install.packages ("car", dep=T) 
library(car)
vif_model = vif(fit)
vif_model

which(round(vif_model)<4)
which(round(vif_model)>4)
which(round(vif_model)==4)
which(round(vif_model)>=4)
