temp = c(42, 37, 46, 30, 50, 43, 43, 46, 46, 49)
pass = c(173, 149, 185, 123, 201, 174, 175, 188, 186, 198)

round(cor(temp, pass), 3)

plot(x=temp,
     y=pass,
     xlab="Temperature (in Fahrenheit) at the beginning of the hour",
     ylab="Number of Passengers",
     main="Plot of Passengers v. Temperature",
     pch=19,
     cex=1,
     col="red",)

fit = lm(pass~temp)
fit$residuals
summary(fit)
