# Reading the data
newspaper_data = read.csv("../Newspaper.csv")

# Getting the summary
summary(newspaper_data)

# Creating an histogram of the Daily circulations
hist(newspaper_data$Daily,
     main="Histogram of Daily Circulations in Thousands",
     xlab="Thousands")

# Creating an histogram of Sunday circulations
hist(newspaper_data$Sunday,
     main="Histogram of Sunday Circulations in Thousands",
     xlab="Thousands")

# Calculating the correlation coefficient
round(cor(newspaper_data$Sunday, newspaper_data$Daily),3)

#Plotting the daily circulation (X) against Sunday circulation (Y) to see the relationship between them.
plot(newspaper_data$Daily,
     newspaper_data$Sunday, 
     main = "Scatter Plot of Daily vs Sunday Circulation",
     xlab = "Daily Circulation (in thousands)", 
     ylab = "Sunday Circulation (in thousands)",
     pch = 16,
     col = "blue")
