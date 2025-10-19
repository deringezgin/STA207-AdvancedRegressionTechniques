# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 13. Adding the categorical variable in the analysis
#
#
#

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
X2 = wine_data$density
X5 = wine_data$color

### Original Model ###
plot(x=X2,
     y=Y,
     main="Density vs. Alcohol Content\nfor Different Categories",
     xlab="Density",
     ylab="Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

##### Plotting the Red and White Wine Observations in the Categorical Model #####
categorical_model = lm(Y ~ X2 + X5)
summary(categorical_model)

white_intercept = categorical_model$coefficients[1] + categorical_model$coefficients[3]
categorical_model$coefficients[1] + categorical_model$coefficients[3]
categorical_model$coefficients[1] 
red_intercept = categorical_model$coefficients[1] 
slope = categorical_model$coefficients[2]

plot(x=X2,
     y=Y,
     main="Density vs. Alcohol Content\n[Model with Categorical Variable]",
     xlab="Density",
     ylab="Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

abline(a = white_intercept, b = slope, col = 1, lwd=4)
abline(a = red_intercept, b = slope, col = 2, lwd=4)

