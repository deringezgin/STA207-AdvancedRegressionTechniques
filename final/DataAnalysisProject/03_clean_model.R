# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 3. Cleaning the influential points from the data and saving it in a new .csv file
#
#
#

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

### Detecting the Influential Points ###
cook = cooks.distance(wine_model)
influential_points = which(cook > 4 / length(Y))
influential_points = unname(influential_points)

### Removing the Influential Points ###
wine_data_cleaned = wine_data[-influential_points, ]

### Save the new dataset in a csv file ###
write.csv(wine_data_cleaned, "winequality_cleaned.csv", row.names = FALSE)
