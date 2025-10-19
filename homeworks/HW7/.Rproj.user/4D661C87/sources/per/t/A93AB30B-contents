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

# Saving the variables
WinPct = MLBStandings2016$WinPct
ERA = MLBStandings2016$ERA
BattingAverage = MLBStandings2016$BattingAverage
Runs = MLBStandings2016$Runs

# Let a reduced model-1 be the one with Hits predictor removed
# from the original model. Report the fitted model.
fit_1 = lm(WinPct ~ ERA + BattingAverage + Runs)
summary(fit_1)

# Checking for multicollinearity
# Scatterplot Matrix
pairs(MLBStandings2016[,c("WinPct","ERA","BattingAverage","Runs")],
      pch=PCH,
      cex=CEX,
      col=COL,
      main="Scatterplot Matrix of Baseball Data (Reduced Model 1)",)

# Plotting the correlation matrix
baseball_cor_1 = cor(MLBStandings2016[,c("WinPct","ERA","BattingAverage","Runs")])
corrplot(baseball_cor_1,
         method = "number",
         main = "Correlation Matrix of Baseball Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff


# Calculate the VIF for each predictor in the model.
# Discuss the results.
vif_model_1 = vif(fit_1)
vif_model_1
summary(fit_1)

# Report ANOVA for the reduced model-1. 
anova(fit_1)


