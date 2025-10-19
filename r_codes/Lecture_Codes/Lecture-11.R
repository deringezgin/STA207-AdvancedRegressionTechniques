###########################
##Categorical Variables##
###########################
#############################
######Dummy Variables#######
## Rcode: explain steps
###P.Kohli
###Lecture-11
############################
data("mtcars")
plot(mpg ~ hp, data = mtcars, cex = 2,main="Scatterplot of Miles per Gallon vs HorsePower")
class(mtcars$am)
plot(mpg ~ hp, data = mtcars, col = am + 1, 
     pch = am + 1, cex = 2, 
     main="Scatterplot of Miles per Gallon vs 
     HorsePower")
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

#Model1:
#SLR
mpg_hp_slr = lm(mpg ~ hp, data = mtcars)
summary(mpg_hp_slr)

plot(mpg ~ hp, data = mtcars, col = am + 1, 
     pch = am + 1, cex = 2)
abline(mpg_hp_slr, lwd = 3, col = "blue")
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

#Model2:
#MLR
mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)
summary(mpg_hp_add)

int_auto = coef(mpg_hp_add)[1]
int_manu = coef(mpg_hp_add)[1] + coef(mpg_hp_add)[3]

slope_auto = coef(mpg_hp_add)[2]
slope_manu = coef(mpg_hp_add)[2]


plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)

abline(int_auto, slope_auto, col = 1, lty = 1, lwd = 2) # add line for auto

abline(int_manu, slope_manu, col = 2, lty = 2, lwd = 2) # add line for manual

legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

##added in class ##
######HW-8####
#part(a)
library(Stat2Data)
data(MLBStandings2016)
names(MLBStandings2016)
class(MLBStandings2016$League)
summary(MLBStandings2016$League)
model2 <- lm(WinPct~ERA+League,data=MLBStandings2016)
summary(model2)
plot(WinPct ~ ERA, data = MLBStandings2016, col = as.numeric(League),pch = as.numeric(League), cex = 2,main="Fig.1 Proportion of Games Won Vs ERA for Two Leagues")
