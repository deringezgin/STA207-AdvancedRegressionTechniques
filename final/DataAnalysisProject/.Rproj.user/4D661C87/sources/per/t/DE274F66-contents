# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 15. Adding all the numerical and categorical variables including the interaction term
#
#
#

### Disable Scientific Notation
options(scipen=999)
### Loading Data ###
wine_data = read.csv("winequality_cleaned.csv")

### Make the color column a factor variable ###
wine_data$color = as.factor(wine_data$color)
wine_data$color = as.numeric(wine_data$color) - 1
numerical_color = as.numeric(wine_data$color) * -1 + 2 # This is for plotting purposes

### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity
X5 = wine_data$color

complete_model = lm(Y ~ X1 + X2 + X3 + X4 + X5 + X2:X5)
summary(complete_model)

par(mfrow=c(2,2))

plot(x=X1,
     y=complete_model$fitted.values,
     main="Residual Sugar vs. Fitted Alcohol Content\n[Model with Numerical & Categorical Variables & Interaction Term]",
     xlab="Residual Sugar",
     ylab="Fitted Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 1))

plot(x=X2,
     y=complete_model$fitted.values,
     main="Density vs. Fitted Alcohol Content\n[Model with Numerical & Categorical Variables & Interaction Term]",
     xlab="Density",
     ylab="Fitted Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

plot(x=X3,
     y=complete_model$fitted.values,,
     main="pH vs. Fitted Alcohol Content\n[Model with Numerical & Categorical Variables & Interaction Term]",
     xlab="pH",
     ylab="Fitted Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

plot(x=X4,
     y=complete_model$fitted.values,,
     main="Fixed Acidity vs. Fitted Alcohol Content\n[Model with Numerical & Categorical Variables & Interaction Term]",
     xlab="Fixed Acidity",
     ylab="Fitted Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

### Hypothesis test of Variables ###
confint(complete_model)
