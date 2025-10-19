#Practice-1
bpm = c(68, 60, 76, 68, 64, 80, 72, 76, 92, 68, 56, 72, 68, 60,
        84, 72, 56, 88, 76, 80, 68, 80, 84, 64, 80, 72, 64, 68, 76, 72)

mbpm = min(bpm)
mbpm
max(bpm)
mean(bpm)
median(bpm)
boxplot(bpm,col="purple",xlab="Pulse Rate of Students",ylab="Beats per minute (BPM)",main="boxplot of pulse rates")
hist(bpm)
sd(bpm)
var(bpm)

#Practice-2
weight = c(60, 72, 57, 90, 95, 72) #weight in KG
height = c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91) #height in meters
bmi = weight/(height^2)
round(bmi,2)
plot(bmi,pch=19,xlab="Subject",ylab="BMI (Kg/sq. m)",col="hotpink",main="BMI of subjects")
plot(weight,height)
cor(weight,height)

####Problem-3
news = read.csv("Newspaper.csv",header=T)
summary(news$Daily)
hist(news$Daily)
summary(news$Sunday)
hist(news$Sunday)
