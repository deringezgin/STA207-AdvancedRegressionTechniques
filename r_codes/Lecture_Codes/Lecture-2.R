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


##HW practice
### problem-2
##Import the data for black-footed albatrosses or enter it manually
gonad = c(42,60,20,96,24,27,27)
age = c(1.42,4.75,0.67,23.64,0.52,2.35,1.4)
#for ease of computations
X = gonad
Y = age

##mean and sd. I have rounded these to three decimals
round(mean(X),3)
round(mean(Y),3)
round(sd(X),3)
round(sd(Y),3)

##covariance and correlation
round(cov(X,Y),3)
round(cor(X,Y),3)

##plot of X versus Y
plot(X,Y,main="Male black-footed albatrosses", xlab="Gonad Size (sq. mm)",ylab="Age (Years)",pch=19,col="red",cex=2)

#Problem: 3
newspaper = read.csv("Newspaper.csv",header=T)
summary(newspaper$Daily)
summary(newspaper$Sunday)
hist(newspaper$Daily)
hist(newspaper$Sunday)

cor.circulation = cor(newspaper$Daily,newspaper$Sunday)
round(cor.circulation,3)

