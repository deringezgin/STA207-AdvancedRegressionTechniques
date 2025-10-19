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
Runs = MLBStandings2016$Runs

# Let a reduced model-2 be the one
# with Hits and Batting Average predictors
# removed from the original model. Report the fitted model.
fit_2 = lm(WinPct ~ ERA + Runs)
summary(fit_2)

# Checking for multicollinearity
# Scatterplot Matrix
pairs(MLBStandings2016[,c("WinPct","ERA","Runs")],
      pch=PCH,
      cex=CEX,
      col=COL,
      main="Scatterplot Matrix of Baseball Data (Reduced Model 2)",)

# Plotting the correlation matrix
baseball_cor_2 = cor(MLBStandings2016[,c("WinPct","ERA","Runs")])
corrplot(baseball_cor_2,
         method = "number",
         main = "Correlation Matrix of Baseball Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff

# Calculate the VIF for each predictor in the model.
# Discuss the results.
vif_model_2 = vif(fit_2)
vif_model_2
# Report ANOVA for reduced model-2.
anova(fit_2)



