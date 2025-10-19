# Adding the Data
gonad_size = c(42, 60, 20, 96, 24, 27, 27)
gonad_age = c(1.42, 4.75, 0.67, 23.64, 0.52, 2.35, 1.4)

# Calculating the standard deviation
round(sd(gonad_size), 3)
round(sd(gonad_age), 3)

# Calculating the mean
round(mean(gonad_size), 3)
round(mean(gonad_age), 3)

# Calculating the correlation coefficient
round(cor(gonad_size, gonad_age), 3)

# Making the scatter plot
plot(gonad_size,
     gonad_age,
     main = "Gonad Size of Black-Footed Albatrosses Related to Their Age",
     xlab = "Gonad Size",
     ylab = "Gonad Age",
     pch = 16,
     col = 'red',
     cex = 1.5,)
