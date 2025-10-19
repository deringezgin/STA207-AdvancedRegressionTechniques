# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 1. Initial Analysis
#
#
#

library(car)
library(lmtest)
library(corrplot)

################## FUNCTION FOR CHECKING LINE CONDITIONS##################

line_assumption_checks = function(y_var, model, model_type, y_lab) {
  # Setting up the grid-space for plotting
  par(mfrow = c(2, 3))
  
  ##### Checking for the Linearity Assumption #####
  # Real vs Fitted Values Plot #
  plot(x = model$fitted.values,
       y = y_var,
       xlab = paste("Fitted", y_lab), 
       ylab = "Real Alcohol Volume Percentage", 
       main = paste("Real vs Fitted", model_type, "\n(Plot A)"))
  abline(h = 0, col = "red", lwd = 2)
  
  # Tukey Test #
  tukey.test = residualPlot(model,
                            main = "Tukey's Curve Test\n(Plot B)",
                            xlab=paste("Fitted", y_lab))
  print(tukey.test)
  
  ##### Checking the Independence Assumption #####
  # Index Plot of Residuals #
  plot(model$residuals, 
       ylab = "Residuals", 
       xlab = "Index", 
       main = "Index Plot of Residuals\n(Plot C)")
  abline(h = mean(model$residuals), col = "blue", lwd = 2)
  
  ##### Checking the Normality Assumption #####
  # Histogram of Residuals #
  hist(model$residuals, 
       main = "Histogram of Residuals\n(Plot D)", 
       xlab = "Residuals", 
       col = "lightblue", 
       border = "black")
  
  # Q-Q Plot #
  qqnorm(model$residuals, main = "Normal Q-Q Plot\n(Plot E)")
  qqline(model$residuals)
  
  ##### Homoscedasticity Assumption #####
  # Residuals vs Fitted Values Plot #
  plot(model, 
       which = 1,
       main=paste("(Plot F)"))
  
  # BP Test #
  bp_test = bptest(model)
  print(bp_test)
  
  # Reset plotting layout
  par(mfrow = c(1, 1))
}


################################################################################

### Loading Data ###
wine_data = read.csv("winequality.csv", sep = ";")

# As the data was manually merged, this shuffles the data -with a set random seed-
# to ensure that there is no bias because of the index of data.
set.seed(42)
wine_data = wine_data[sample(nrow(wine_data)), ]
PCH = 19
CEX = 0.3


### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

### Scatterplot Matrix ###
pairs(wine_data[, c("alcohol", "residual.sugar", "density", "pH", "fixed.acidity")],
      pch = PCH,
      cex = CEX,)

### Correlation Matrix ###
wine_cor = cor(wine_data[, c("alcohol", "residual.sugar", "density", "pH", "fixed.acidity")])
corrplot(wine_cor,
         method = "number",
         main = "Correlation Matrix of Wine Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff

### Fitting an MLR Model with all the predictors and the response variable ###
wine_model = lm(Y ~ X1 + X2 + X3 + X4)
summary(wine_model)

### Plot the residual plot ### 
plot(x=wine_model$fitted.values,
     y=wine_model$residuals,
     xlab="Fitted Alcohol Content Values",
     ylab="Residuals",
     main="Residuals vs Fitted Values of Alcohol Content",
     pch=PCH, cex=CEX)

plot(wine_model,
     which=1,
     main="Residuals vs Fitted Values of Alcohol Content")

### Checking the confidence intervals of the model for the significance of predictors ###
confint(wine_model)

line_assumption_checks(Y, wine_model, "Alcohol Volume Percentage", "Alcohol Volume Percentage")
