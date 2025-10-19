library(Stat2Data) # Importing the library
data("Fluorescence")  # Importing the data
?Fluorescence

# Calculating the correlation coefficient
round(cor(Fluorescence$Calcium, Fluorescence$ProteinProp), 3)

# Making a scatter plot
plot(x=Fluorescence$Calcium,
     y=Fluorescence$ProteinProp,
     xlab="Calcium Log of Free Calcium",
     ylab="Proportion of Protein Bound to Calcium",)

fit = lm(Fluorescence$ProteinProp ~ Fluorescence$Calcium) 
summary(fit)
plot(x=Fluorescence$Calcium,
     y=Fluorescence$ProteinProp,
     xlab="Calcium Log of Free Calcium",
     ylab="Proportion of Protein Bound to Calcium",)

abline(fit,col="red",lwd=4)
