# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 5. Applying the log Transformation to response and/or predictor variables
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
PCH = 19
CEX = 0.3


### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

# Log Transformation #
### 1. Taking the log of the response variable ###
log_response_model = lm(log(Y) ~ X1 + X2 + X3 + X4)
summary(log_response_model)

line_assumption_checks(log(Y), log_response_model, "Log of Alcohol Volume Percentage", "Log of Alcohol Volume Percentage")

### 2. Taking the log of the  predictor variables ###
log_predictor_model = lm(Y ~ log(X1) + log(X2) + log(X3) + log(X4))
summary(log_predictor_model)

line_assumption_checks(Y, log_predictor_model, "Alcohol Volume Percentage\n[Log of Predictor Variables]", "Alcohol Volume Percentage")

### 3. Model with both response and predictor variables as log transformed ###

log_complete_model = lm(log(Y) ~ log(X1) + log(X2) + log(X3) + log(X4))
summary(log_complete_model)

line_assumption_checks(log(Y), log_complete_model, "Log of Alcohol Volume Percentage\n[Log of Predictor Variables]", "Log of Alcohol Volume Percentage")
