weight = c(60, 72, 57, 90, 95, 72)
height = c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)

bmi = weight / (height * height)
round(bmi, 2)
boxplot(bmi,
        col='blue',
        main='Boxplot of BMI',
        notch=TRUE)
plot(weight, height)
cor(weight, height)
