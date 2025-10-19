# hw-3 R code
# Date: 9/12/24
# P.Kohli
#Problem:1
#install.packages("Stat2Data")
library(Stat2Data)
data("Fluorescence")
#?Fluorescence
X=Fluorescence$Calcium
Y=Fluorescence$ProteinProp
cor(X,Y)
fit = lm(Y~X)
summary(fit)
