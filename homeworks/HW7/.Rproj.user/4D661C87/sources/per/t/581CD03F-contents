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

fit_0 = lm(WinPct ~ ERA + BattingAverage + Runs + Hits)
fit_1 = lm(WinPct ~ ERA + BattingAverage + Runs)
fit_2 = lm(WinPct ~ ERA + Runs)

# Getting the Adjusted R-squared values
summary(fit_0)$adj.r.squared
summary(fit_1)$adj.r.squared
summary(fit_2)$adj.r.squared

# Getting the AIC/AICc/BIC values
n = nrow(MLBStandings2016)
aic_0 = AIC(fit_0)/n
r_0 = length(fit_0$coefficients)
sse_0 = sum(resid(fit_0)^2)
aicc_0 = log(sse_0/n) + (n + r_0)/(n - r_0 - 2)
bic_0 = (AIC(fit_0, k = log(n)))/n

aic_1 = AIC(fit_1)/n
r_1 = length(fit_1$coefficients)
sse_1 = sum(resid(fit_1)^2)
aicc_1 = log(sse_1/n) + (n + r_1)/(n - r_1 - 2)
bic_1 = (AIC(fit_1, k = log(n)))/n

aic_2 = AIC(fit_2)/n
r_2 = length(fit_2$coefficients)
sse_2 = sum(resid(fit_2)^2)
aicc_2 = log(sse_2/n) + (n + r_2)/(n - r_2 - 2)
bic_2 = (AIC(fit_2, k = log(n)))/n

aic_0
aic_1
aic_2

aicc_0
aicc_1
aicc_2

bic_0
bic_1
bic_2

# Get the residual standard error
summary(fit_0)
summary(fit_1)
summary(fit_2)



