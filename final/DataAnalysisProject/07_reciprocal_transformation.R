# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 7. Applying the reciprocal Transformation to response and/or predictor variables
#
#
#

library(car)
library(lmtest)

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
wine_data = read.csv("winequality_cleaned.csv")

# As the data was manually merged, this shuffles the data -with a set random seed-
# to ensure that there is no bias because of the index of data.
PCH = 19
CEX = 0.3


### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

### 1. Taking the reciprocal of the response variable ###
reciprocal_Y = 1/Y
reciprocal_X1 = 1/X1
reciprocal_X2 = 1/X2
reciprocal_X3 = 1/X3
reciprocal_X4 = 1/X4

reciprocal_response_model = lm(reciprocal_Y ~ X1 + X2 + X3 + X4)
summary(reciprocal_response_model)

line_assumption_checks(1/Y, reciprocal_response_model, "Reciprocal of Alcohol Volume Percentage", "Reciprocal of Alcohol Volume Percentage")

### 2. Taking the reciprocal of the predictor variables ###

reciprocal_predictor_model = lm(Y ~ reciprocal_X1 + reciprocal_X2 + reciprocal_X3 + reciprocal_X4)
summary(reciprocal_predictor_model)
line_assumption_checks(Y, reciprocal_predictor_model, "Alcohol Volume Percentage\n[Reciprocal of Predictor Variables]", "Alcohol Volume Percentage")

### 3. Model with both response and predictor variables as reciprocal transformed ###
reciprocal_complete_model = lm(reciprocal_Y ~ reciprocal_X1 + reciprocal_X2 + reciprocal_X3 + reciprocal_X4)
summary(reciprocal_complete_model)
line_assumption_checks(1/Y, reciprocal_complete_model, "Reciprocal of Alcohol Volume Percentage\n[Reciprocal of Predictor Variables]", "Reciprocal of Alcohol Volume Percentage")
