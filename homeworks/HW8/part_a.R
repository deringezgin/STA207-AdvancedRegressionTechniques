# Reading the MLB data
library(Stat2Data)
data(MLBStandings2016)
names(MLBStandings2016)
class(MLBStandings2016$League)
summary(MLBStandings2016$League)

# Regress WinPct on ERA and League
model_1 = lm(WinPct ~ ERA + League, data = MLBStandings2016)
summary(model_1)

# Plotting the Data with Different Categories
plot(WinPct ~ ERA,
     data = MLBStandings2016,
     col = as.numeric(League),
     pch = as.numeric(League),
     cex = 2,
     main="Fig.1 Proportion of Games Won v. ERA for Two Leagues")


# Plotting the lines for individual categorical variables

# Extracting the coefficients

american_intercept = coef(model_1)[1]
national_intercept = coef(model_1)[1] + coef(model_1)[3]

american_slope = coef(model_1)[2]
national_slope = coef(model_1)[2]

# Adding the lines
abline(american_intercept, american_slope, col = 1, lty = 1, lwd = 2) # add line for American League
abline(national_intercept, national_slope, col = 2, lty = 2, lwd = 2) # add line for National League

legend("topright", c("American League", "National League"), col = c(1, 2), pch = c(1, 2))
