# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 2. Investigating Leverage, Outlier, and Influential Points
#
#
#

library(car)


### Loading Data ###
wine_data = read.csv("winequality.csv", sep = ";")

# As the data was manually merged, this shuffles the data -with a set random seed-
# to ensure that there is no bias because of the index of data.
set.seed(42)
wine_data = wine_data[sample(nrow(wine_data)), ]


### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity

### Fitting the Model ###
wine_model = lm(Y ~ X1 + X2 + X3 + X4)

### Detecting the Leverage Points ###
h_values = hatvalues(wine_model)
high_levarage_points = which(h_values > 2 * mean(h_values))
high_levarage_points = unname(high_levarage_points)
high_levarage_count = length(high_levarage_points)
high_levarage_count
high_levarage_points

### Detecting the Outliers ###
outliers = which(rstudent(wine_model) > 2 | rstudent(wine_model) < -2)
outliers = unname(outliers)
outlier_count = length(outliers)
outlier_count
outliers

par(mfrow = c(1,2))

### Detecting the Influential Points ###
cook = cooks.distance(wine_model)
plot(cook, pch = 19, cex=1, main = "Cook's Distance Plot")
influential_points = which(cook > 4 / length(Y))
influential_points = unname(influential_points)
influential_count = length(influential_points)
influencePlot(wine_model)
influential_points
