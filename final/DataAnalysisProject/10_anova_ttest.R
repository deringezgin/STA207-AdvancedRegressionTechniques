# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 10. Reporting the ANOVA of the model and 
#
#
#

library(car)

### Loading Data ###
wine_data = read.csv("winequality_cleaned.csv")

### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

### Fitting the Model ###
wine_model = lm(Y ~ X1 + X2 + X3 + X4)

summary(wine_model)

### Report ANOVA of model ###
anova(wine_model)

### Discuss if all predictors are statistically significant or not (t-test).###
confint(wine_model)
