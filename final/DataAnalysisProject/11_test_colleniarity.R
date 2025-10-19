# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 11. Choosing which predictor to remove
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

### Fitting the Model ###
wine_model = lm(Y ~ X1 + X2 + X3 + X4)

### Highly correlated and statistically insignificant predictors ###
vif_values = vif(wine_model)
vif_values

### Scatterplot Matrix ###
pairs(wine_data[, c("alcohol", "residual.sugar", "density", "pH", "fixed.acidity")],
      pch = PCH,
      cex = CEX,
      main="Scatterplot Matrix for the Linear Regression Model")

### Correlation Matrix ###
wine_cor = cor(wine_data[, c("alcohol", "residual.sugar", "density", "pH", "fixed.acidity")])
corrplot(wine_cor,
         method = "number",
         main = "Correlation Matrix of Wine Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff
