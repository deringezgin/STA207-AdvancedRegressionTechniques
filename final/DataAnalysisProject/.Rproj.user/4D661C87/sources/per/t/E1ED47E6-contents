# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 13. Adding the interaction term to the model
#
#
#

options(scipen=999)

### Loading Data ###
wine_data = read.csv("winequality_cleaned.csv")

### Make the color column a factor variable ###
wine_data$color = as.factor(wine_data$color)
levels(wine_data$color)
wine_data$color = as.numeric(wine_data$color) - 1

### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity
X5 = wine_data$color

### Plotting the fitted Red Wine and White Wine Separately ###
numerical_color = as.numeric(wine_data$color) * -1 + 2 # This is for plotting purposes

##### Plotting the Red and White Wine Observations in the Original Model #####
interaction_model = lm(Y ~ X2 + X5 + X2:X5)
summary(interaction_model)

white_intercept = interaction_model$coefficients[1] + interaction_model$coefficients[3]
red_intercept = interaction_model$coefficients[1] 
red_slope = interaction_model$coefficients[2]
white_slope = interaction_model$coefficients[2] + interaction_model$coefficients[4]

plot(x=X2,
     y=Y,
     main="Density vs. Alcohol Content\n[Model with Interaction Term]",
     xlab="Density",
     ylab="Alcohol Content",
     col = numerical_color,
     pch=numerical_color,
     cex=0.5,)
legend("topright", c("White Wine", "Red Wine"), col = c(1, 2), pch = c(1, 2))

abline(a = white_intercept, b = white_slope, col = 1, lwd=4)
abline(a = red_intercept, b = red_slope, col = 2, lwd=4)
