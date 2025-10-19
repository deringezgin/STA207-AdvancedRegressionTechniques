######################
##Unusual Obervations#
######################
###Example ###
par(mfrow = c(1, 3))
set.seed(1000)
ex_data  = data.frame(x = 1:10,
                      y = 10:1 + rnorm(n = 10))
ex_model = lm(y ~ x, data = ex_data)
summary(ex_model)
# low leverage, large residual, small influence
point_1 = c(5.4, 11)
ex_data_1 = rbind(ex_data, point_1)
model_1 = lm(y ~ x, data = ex_data_1)
summary(model_1)
plot(y ~ x, data = ex_data_1, cex = 2, pch = 20, col = "grey",
     main = "Low Leverage, Large Residual, Small Influence")
points(x = point_1[1], y = point_1[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_1, lty = 2, col = "darkorange", lwd = 2)
legend("topleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, small residual, small influence
point_2 = c(18, -5.7)
ex_data_2 = rbind(ex_data, point_2)
model_2 = lm(y ~ x, data = ex_data_2)
summary(model_2)
plot(y ~ x, data = ex_data_2, cex = 2, pch = 20, col = "grey",
     main = "High Leverage, Small Residual, Small Influence")
points(x = point_2[1], y = point_2[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_2, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))

# high leverage, large residual, large influence
point_3 = c(14, 5.1)
ex_data_3 = rbind(ex_data, point_3)
model_3 = lm(y ~ x, data = ex_data_3)
summary(model_3)
plot(y ~ x, data = ex_data_3, cex = 2, pch = 20, col = "grey", ylim = c(-3, 12),
     main = "High Leverage, Large Residual, Large Influence")
points(x = point_3[1], y = point_3[2], pch = 1, cex = 4, col = "black", lwd = 2)
abline(ex_model, col = "dodgerblue", lwd = 2)
abline(model_3, lty = 2, col = "darkorange", lwd = 2)
legend("bottomleft", c("Original Data", "Added Point"),
       lty = c(1, 2), col = c("dodgerblue", "darkorange"))
###Leverage points detection
2*mean(hatvalues(ex_model))
which(hatvalues(ex_model)>(2*mean(hatvalues(ex_model))))
hatvalues(model_1)
2*mean(hatvalues(model_1))
which(hatvalues(model_1)>(2*mean(hatvalues(model_1))))
hatvalues(model_2)
2*mean(hatvalues(model_2))
which(hatvalues(model_2)>(2*mean(hatvalues(model_2))))
hatvalues(model_3)
2*mean(hatvalues(model_3))
which(hatvalues(model_3)>(2*mean(hatvalues(model_3))))
##without printing the values
hvalues1 = hatvalues(ex_model)
hvalues2 = hatvalues(model_1)
hvalues3 = hatvalues(model_2)
hvalues4 = hatvalues(model_3)
hvaluesthresh1 = 2*mean(hatvalues(ex_model))
hvaluesthresh2 = 2*mean(hatvalues(model_1))
hvaluesthresh3 = 2*mean(hatvalues(model_2))
hvaluesthresh4 = 2*mean(hatvalues(model_3))
index.leverage1 = which(hvalues1>hvaluesthresh1)
index.leverage2 = which(hvalues2>hvaluesthresh2)
index.leverage3 = which(hvalues3>hvaluesthresh3)
index.leverage4 = which(hvalues4>hvaluesthresh4)
length(index.leverage1)
length(index.leverage2)
length(index.leverage3)
length(index.leverage4)

###Outliers
rstandard(ex_model)
rstandard(model_1)
rstandard(model_2)
rstandard(model_3)

##not printing all
z1 = rstandard(ex_model)
z2 = rstandard(model_1)
z3 = rstandard(model_2)
z4 = rstandard(model_3)
index.resid1 = which(abs(z1)>2)
index.resid2 = which(abs(z2)>2)
index.resid3 = which(abs(z3)>2)
index.resid4 = which(abs(z4)>2)
length(index.resid1)
length(index.resid2)
length(index.resid3)
length(index.resid4)
###Influential
cooks.distance(ex_model)
cooks.distance(model_1)
cooks.distance(model_2)
cooks.distance(model_3)
#To check influential
which(cooks.distance(model_1) > (4 /11))
which(cooks.distance(model_2) > (4 /11))
which(cooks.distance(model_3) > (4 /11))

#without printing
cd1 = cooks.distance(ex_model)
cd2 = cooks.distance(model_1)
cd3 = cooks.distance(model_2)
cd4 = cooks.distance(model_3)

index.cd1 = which(cd1>4/10)
index.cd2 = which(cd2>4/11)
index.cd3 = which(cd3>4/11)
index.cd4 = which(cd4>4/11)
length(index.cd1)
length(index.cd2)
length(index.cd3)
length(index.cd4)

par(mfrow=c(2,2))
plot(ex_model, pch=16, which=1)#Residual Plot
plot(model_1, pch=16, which=4)
plot(model_2, pch=16, which=4)
plot(model_3, pch=16, which=4)
 dev.off()
 #install.packages("car)
 library(car)
par(mfrow=c(2,2))
influencePlot(ex_model)
influencePlot(model_1)
influencePlot(model_2)
influencePlot(model_3)
dev.off()

