# Importing the libraries
library(ISLR)
library(corrplot)
library(Stat2Data)
library(car)

# Global Variables
PCH = 19
CEX = 0.5
COL = 'black'

# Reading the data
data(MLBStandings2016)
attach(MLBStandings2016)
# names(MLBStandings2016)

# Saving the variables
Y = MLBStandings2016$WinPct
X1 = MLBStandings2016$ERA
X2 = MLBStandings2016$BattingAverage
X3 = MLBStandings2016$Runs
X4 = MLBStandings2016$Hits

FM = lm(Y ~ X1 + X2 + X3 + X4)
RM1 = lm(Y ~ X1 + X2 + X3)
RM2 = lm(Y ~ X1 + X3)

# Comparing the models
anova(FM, RM1)
# H0: B4 = 0
# Ha: B4 != 0
# alpha = 0.05
# F = 0.2087
# p-value = 0.6517
# p-Value > 0.05 --> Do not reject H0
# As we are not rejecting the smaller model,
# At 5 percent significance, hits is not helpful in predicting
# the win percentage

anova(FM, RM2)
# H0: B2 = 0 and B4 = 0
# Ha: At least one of B2 and B4 != 0
# alpha = 0.05
# F = 1.1407
# P-Value 0.3357
# p-Value > 0.05 --> Do not reject H0
# As we are not rejecting the smaller model,
# At 5 percent significance, batting average and hits
# are not helpful in predicting the win percentage

