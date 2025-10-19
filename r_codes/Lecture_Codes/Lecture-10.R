######################################################
###Lecture-10 R code with examples and practice ###
##Written by:P.Kohli
##Date: 11/12/24
######################################################
#################################################
#ANOVA for Galton Data 
#################################################
library(UsingR) #library where galton data is available
#Linear model for Galton
model_1 <- lm(child~parent, data=galton)
summary(model_1)
anova(model_1)
##############################################
#autompg data
#################################################
#read autompg data
autompg <- read.csv("autompg.csv")
#####Model 1: mpg as a function of cyl and hp
model1 <-  lm(mpg~cyl+hp,data=autompg)
#####Model 2: mpg as a function of cyl and wt
model2 <-  lm(mpg~cyl+wt,data=autompg)
#####Model 3: mpg as a function of cyl, wt, and hp
model3 <-  lm(mpg~cyl+hp+wt,data=autompg)
#####Model 4: mpg as a function of cyl, wt, hp, and acc
model4 <-  lm(mpg~cyl+hp+wt+acc,data=autompg)
#################################################
#ANOVA for all these models
#################################################
anova(model1)
anova(model2)
anova(model3)
anova(model4)

##############################################
###### F-test for Model1 vs Model3 ###########
##############################################
anova(model1,model3)

##############################################
###### F-test for Model2 vs Model4 ###########
##############################################
anova(model2,model4)

