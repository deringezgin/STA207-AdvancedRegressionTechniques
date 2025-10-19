# Reading the MLB data
library(Stat2Data)
data(MLBStandings2016)

model = lm(WinPct ~ ERA + League + ERA:League, data = MLBStandings2016)
summary(model)

# Plotting the Data with Different Categories
american_intercept = coef(model)[1]
national_intercept = coef(model)[1] + coef(model)[3]

american_slope = coef(model)[2]
national_slope = coef(model)[2] + coef(model)[4]
plot(WinPct ~ ERA,
     data = MLBStandings2016,
     col = as.numeric(League),
     pch = as.numeric(League),
     cex = 2,
     main="Fig.2 Proportion of Games Won v. ERA for Two Leagues")
abline(american_intercept, american_slope, col = 1, lty = 1, lwd = 2) # add line for American League
abline(national_intercept, national_slope, col = 2, lty = 2, lwd = 2) # add line for National League
legend("topright", c("American League", "National League"), col = c(1, 2), pch = c(1, 2))
