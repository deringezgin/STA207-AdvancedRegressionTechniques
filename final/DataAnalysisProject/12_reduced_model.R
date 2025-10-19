# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 12. Creating the reduced model
# Comparing two models
#
#
#

library(car)
library(lmtest)
library(corrplot)
PCH=19
CEX=0.2

### Loading Data ###
wine_data = read.csv("winequality_cleaned.csv")

### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

wine_model = lm(Y ~ X1 + X2 + X3 + X4)

### Removing the highly correlated and statistically insignificant predictor (X2) ###
reduced_wine_model = lm(Y ~ X2 + X3 + X4)

##### Comparing the Models #####
### Necessary Information for the Full Model ################################

summary(wine_model)

# Confidence Intervals
confint(wine_model)

# AIC AICc BIC values
n = nrow(wine_data)
r = length(coefficients(wine_model))
sse = sum(residuals(wine_model)^2)

full_model_aic = AIC(wine_model) / n
full_model_aicc = log(sse/n) + ((n + r)/(n - r - 2))
full_model_bic = AIC(wine_model, k = log(n))/n

### Necessary Information for the Reduced Model ################################
summary(reduced_wine_model)

# Confidence Intervals
confint(reduced_wine_model)

anova(reduced_wine_model)

# AIC AICc BIC values
n = nrow(wine_data)
r = length(coefficients(reduced_wine_model))
sse = sum(residuals(reduced_wine_model)^2)

reduced_model_aic = AIC(reduced_wine_model) / n
reduced_model_aicc = log(sse/n) + ((n + r)/(n - r - 2))
reduced_model_bic = AIC(reduced_wine_model, k = log(n))/n

### Anova Test ###
anova(reduced_wine_model, wine_model)

