### Disable Scientific Notation
options(scipen=999)
# Reading the MLB data
library(Stat2Data)
data(MLBStandings2016)

# The full model
model = lm(WinPct ~ ERA + Runs + ERA:Runs, data = MLBStandings2016)
summary(model)
# The reduced model
model_rm = lm(WinPct ~ ERA + Runs, data = MLBStandings2016)
summary(model_rm)

anova(model_rm, model)