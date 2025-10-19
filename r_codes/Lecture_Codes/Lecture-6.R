####################################
#use initech data from Lecture-6
###################################
#Make sure you save initech.csv data is in the same folder (STA-207)
####initech example####
#read data
initech <- read.csv("data/initech.csv",header = T)
#scatterplot
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
#fitted model
initech_fit = lm(salary ~ years, data = initech)
summary(initech_fit)
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
abline(initech_fit, col = "darkorange", lwd = 2)

par(mfrow=c(2,2))
plot(x=initech$years, y=initech$salary,main="(a)",xlab="years",ylab="salary ($)")
plot(initech_fit$fitted.values,y=initech$salary,xlab="(Fitted Values)",ylab="Y values",main="(b)", pch=19)
plot(initech_fit$fitted.values,initech_fit$residuals,xlab="(Fitted values)",ylab="Residuals",main="(c)", pch=19)
library(car)
Tukey.test <- residualPlot(initech_fit,pch=17,main="(d)")
Tukey.test

par(mfrow=c(2,2))
hist(initech_fit$residuals,main="(a)",xlab = "Residuals")
qqnorm(initech_fit$residuals,pch=19,main="(b)")
plot(initech_fit$residuals,pch=19,ylab="Residuals",main="(c)")
##Residual Plot
plot(initech$years,initech_fit$residuals,pch=19,xlab="Years",ylab="Residuals",main="(d)")
abline(h=mean(initech_fit$residuals),col="blue",lwd=2)
dev.off()
#BP test
#install.packages("lmtest")
library(lmtest)
bptest(initech_fit) 


###log(Y): y is salary
#scatterplot
Y = log(initech$salary)
plot(Y ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Log Salaries at Initech, By Seniority")
#fitted model
initech_logfit = lm(Y ~ years, data = initech)
summary(initech_logfit)
plot(Y ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,ylab="log(salary)",
     main = "Log Salaries at Initech, By Seniority")
abline(initech_logfit, col = "darkorange", lwd = 2)
#residual plot
plot(initech$years,initech_logfit$residuals,pch=19,xlab="Years",ylab="Residuals",main="Residual plot",col="grey")
abline(h=mean(initech_logfit$residuals),col="orange",lwd=2)
###plot on the original scale
plot(salary ~ years, data = initech, col = "grey", pch = 20, cex = 1.5,
     main = "Salaries at Initech, By Seniority")
curve(exp(initech_logfit$coef[1] + initech_logfit$coef[2] * x),
      from = 0, to = 30, add = TRUE, col = "darkorange", lwd = 2)
#Line assumptions
par(mfrow=c(2,2))
plot(x=initech$years, y=Y,main="(a)",xlab="years",ylab="log(salary)")
plot(initech_logfit$fitted.values,y=Y,xlab="(Fitted Values)",ylab="log salary values",main="(b)", pch=19)
plot(initech_logfit$fitted.values,initech_logfit$residuals,xlab="(Fitted values)",ylab="Residuals",main="(c)", pch=19)
library(car)
Tukey.test <- residualPlot(initech_logfit,pch=17,main="(d)")
Tukey.test

# par(mfrow=c(2,2))
hist(initech_logfit$residuals,main="(a)",xlab = "Residuals")
qqnorm(initech_logfit$residuals,pch=19,main="(b)")
plot(initech_logfit$residuals,pch=19,ylab="Residuals",main="(c)")
##Residual Plot
plot(initech$years,initech_logfit$residuals,pch=19,xlab="Years",ylab="Residuals",main="(d)")
abline(h=mean(initech_logfit$residuals),col="blue",lwd=2)
dev.off()
#BP test
#install.packages("lmtest")
library(lmtest)
bptest(initech_logfit) 


######################
###Polynomial Example
######################
marketing <- read.csv("data/marketing.csv")
plot(sales ~ advert, data = marketing, 
     xlab = "Advert Spending (in $10,000)", ylab = "Sales (in $10,000)",
     pch = 20, cex = 2,main="Fig 4(a): scatterplot")
#############
#Linear model#
##############

mark_mod = lm(sales ~ advert, data = marketing)
summary(mark_mod)
plot(sales ~ advert, data = marketing, 
     xlab = "Advert Spending (in $10,000)", ylab = "Sales (in $10,000)",
     pch = 20, cex = 2,main="Fig 4(b): scatterplot")
abline(mark_mod,col="red",lwd=2)
##################
#quadratic model#
##################
#X and X^2 model
mark_mod_poly2 = lm(sales ~ advert + I(advert ^ 2), data = marketing) 
summary(mark_mod_poly2)
##Make a plot with model1 and 2 
plot(sales ~ advert, data = marketing, 
     xlab = "Advert Spending (in $10,000)", ylab = "Sales (in $10,000)",
     pch = 20, cex = 2,main="Fig 4(c): Linear and Quadratic Fits")
abline(mark_mod, lty = 2, col = "darkgreen", lwd = 3)
xplot = seq(0, 16, by = 0.01)
lines(xplot, predict(mark_mod_poly2, newdata = data.frame(advert = xplot)),
      col = "blue", lwd = 3)
legend("bottomright",legend=c("Linear","Quadratic"),col=c("darkgreen","blue"),lty=c(2,1),lwd=c(2,2))

#############
#cubic model#
##############
mark_mod_poly3 = lm(sales ~ advert + I(advert ^ 2) + I(advert ^ 3), data = marketing)
mark_mod_poly4 = lm(sales ~ advert + I(advert ^ 2) + I(advert ^ 3) + I(advert ^ 4), data = marketing)
summary(mark_mod_poly4)
summary(mark_mod_poly3)
plot(sales ~ advert, data = marketing, 
     xlab = "Advert Spending (in $10,000)", ylab = "Sales (in $10,000)",
     pch = 20, cex = 2,main="Fig 4(d): Linear, Quadratic, and Cubic Fits")
abline(mark_mod, lty = 2, col = "darkgreen", lwd = 3)
xplot = seq(0, 16, by = 0.01)
lines(xplot, predict(mark_mod_poly2, newdata = data.frame(advert = xplot)),
      col = "blue", lwd = 3)
lines(xplot, predict(mark_mod_poly3, newdata = data.frame(advert = xplot)),
      col = "red", lty = 3, lwd = 3)
lines(xplot, predict(mark_mod_poly4, newdata = data.frame(advert = xplot)),
      col = "orange", lty = 3, lwd = 3)
legend("bottomright",legend=c("Linear","Quadratic","Cubic", "4th Order"),col=c("darkgreen","blue","red", "orange"),lty=c(2,1,2),lwd=c(2,2,2))

library(ggplot2)
ggplot(data = marketing, aes(x = advert, y = sales)) +
  stat_smooth(method = "lm", se = FALSE, color = "green", formula = y ~ x) +
  stat_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x + I(x ^ 2)) +
  stat_smooth(method = "lm", se = FALSE, color = "red", formula = y ~ x + I(x ^ 2)+ I(x ^ 3)) +
  stat_smooth(method = "lm", se = FALSE, color = "orange", formula = y ~ x + I(x ^ 2)+ I(x ^ 3) + I(x ^ 4), lwd=3) +
  geom_point(colour = "black", size = 3)
legend("bottomright",legend=c("Linear","Quadratic","Cubic"),col=c("darkgreen","blue","red", "orange"),lty=c(2,1,2),lwd=c(2,2,2))

######centering example
library(UsingR)
fit.orig = lm(galton$child~galton$parent)
summary(fit.orig)
#centering mid-parent height
x.center = scale(galton$parent,center=TRUE,scale=FALSE)
fit.center <- lm(galton$child~x.center)
summary(fit.center)
par(mfrow = c(1, 2))
plot(x=galton$parent, y=galton$child,main="Original",xlab="midparent height",ylab="Child's height")
abline(fit.orig,col="orange",lwd=3)
plot(x=x.center, y=galton$child,main="Centered",xlab="centered midparent height",ylab="Child's height")
abline(fit.center,col="orange",lwd=3)
abline(v=0,col="blue",lwd=2,lty=2)
abline(h=68.08,col="blue",lwd=2,lty=2)
