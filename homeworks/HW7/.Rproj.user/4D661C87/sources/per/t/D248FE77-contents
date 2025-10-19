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
WinPct = MLBStandings2016$WinPct
ERA = MLBStandings2016$ERA
BattingAverage = MLBStandings2016$BattingAverage
Runs = MLBStandings2016$Runs
Hits = MLBStandings2016$Hits

# Part (a)
# Make scatterplot matrix and correlation matrix for
# WinPct , ERA, BattingAverage, Runs, and Hits.
# Discuss the relationship between each pair of variables.
pairs(MLBStandings2016[,c("WinPct","ERA","BattingAverage","Runs","Hits")],
      pch=PCH,
      cex=CEX,
      col=COL,
      main="Scatterplot Matrix of Baseball Data",)

# Plotting the correlation matrix
baseball_cor = cor(MLBStandings2016[,c("WinPct","ERA","BattingAverage","Runs","Hits")])

corrplot(baseball_cor,
         method = "number",
         main = "Correlation Matrix of Baseball Data",
         mar = c(0, 0, 2, 0))  # Adjust top margin to avoid title cutoff

# Part (b)
# Regressing WinPct on the four predictors
# ERA, BattingAverage, Runs, and Hits and report the fitted model. 
fit = lm(WinPct ~ ERA + BattingAverage + Runs + Hits)
summary(fit)

# Part (c)
# Checking for multicollinearity
# Calculate the VIF for each predictor in the model.
# Discuss the results.
vif_model = vif(fit)
vif_model

which(round(vif_model)<4)
which(round(vif_model)>4)
which(round(vif_model)==4)
which(round(vif_model)>=4)

# Part (d)
# Report ANOVA of the above model. 
test = anova(fit)
test
summary(fit)
