# Derin Gezgin | Johnny Andreasen | Sababa Ahmed
# Fall 2024 | STA 207: Advanced Regression Techniques
# Data Analysis Project | Complete Code
# 0. Setup. Loading the data and saving the variables
#
#
#

### Loading Data ###
wine_data = read.csv("winequality.csv", sep = ";")

# As the data was manually merged, this shuffles the data -with a set random seed-
# to ensure that there is no bias because of the index of data.
set.seed(42)
wine_data = wine_data[sample(nrow(wine_data)), ]
PCH = 19
CEX = 0.3


### Response Variable ###
Y = wine_data$alcohol

### Predictor Variables ###
X1 = wine_data$residual.sugar
X2 = wine_data$density
X3 = wine_data$pH
X4 = wine_data$fixed.acidity
X5 = wine_data$color