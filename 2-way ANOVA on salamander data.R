#Maya Wade Assignment 7 BIOL 2701
#Tested most recently on R version 4.3.1 (2023-06-16) -- "Beagle Scouts"

#Scenario – A research team ran an experiment on the effect of photoperiod and temperature on growth of larval salamanders. They used 2 photoperiods, 16L:8D and 12L:12D, and 2 temperature conditions, low (15˚C) and high (20˚C). Salamander larvae were reared in 4 separate pens for each treatment combination. Growth rate was calculated by measuring the mass of each larvae from each pen at the start of the experiment and subtracting this from the mass at the end of the experiment and then dividing this by the experiment duration. Doing this they were able to calculate the mean growth rate (mg/day) gained by the larval salamanders from each pen under each treatment combination. The data are stored in salamander.csv. 

#Use this dataset to test the effect of photoperiod and temperature on larval salamander growth rates.

data1<-read.csv(file.choose(), header=TRUE)
str(data1)
names(data1)


#Reordering factor levels for temperature so "low" comes before "high"
data1$temp=factor(data1$temp, levels=c("low","high"))
levels(data1$temp)

#Reordering factor levels for photoperiod
data1$photo=factor(data1$photo, levels=c("12L","16L"))
levels(data1$photo)

#installing needed packages
library(sciplot)
library(car)
library(psych)

#Calculating mean, standard error (SE), and standard deviation (SD) of treatments
mean<-tapply(data1$growthrate, list(data1$temp, data1$photo), mean)
mean

SE<-tapply(data1$growthrate, list(data1$temp, data1$photo), se)
SE

SD<-tapply(data1$growthrate, list(data1$temp, data1$photo), sd)
SD

#Making barplot with error bars. Note that cex.main makes the font size of the main title slightly smaller than default.
plotTop <- max(mean+SE*9) 
barCenters=barplot(mean, beside=T, legend=T, ylim=c(0,plotTop), axis.lty=1, main="Effect of Temperature and Photoperiod on Growth Rate of Larval Salamanders", ylab="Growth Rate (mg/day)", xlab="Photoperiod", las=1, cex.main=0.95)
arrows(barCenters, mean-SE, barCenters, mean+SE, lwd=1, angle=90, code=3) 
box()


#Making interaction plot
lineplot.CI(x.factor=data1$photo, response=data1$growthrate,group=data1$temp, trace.label="Temperature", xlab="Photoperiod", ylab="Growth Rate (mg/day)", main="Effect Temperature and Photoperiod on Growth Rate of Larval Salamanders", cex.main=0.95)


#Building ANOVA model
model1 = aov(growthrate~temp*photo,data=data1)

#testing assumptions of ANOVA visually
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))

#testing normality using Shapiro-wilk. I have extracted the residuals in the same line
shapiro.test(resid(model1))

#testing homogeneity of variance 3 different ways
bartlett.test(data1$growthrate ~ interaction (data1$temp, data1$photo), data=data1)
leveneTest(data1$growthrate ~ interaction (data1$temp, data1$photo), data=data1)
fligner.test(data1$growthrate ~ interaction (data1$temp, data1$photo), data=data1)

#data passes assumptions so I can move on to interpretation of output

summary(model1)

#Based on summary of model1, the degrees of freedom for MSerror = 28. Degrees of freedom for MSinteraction = 1. 

#Calculating the total sums of squares now.
SStotal=11.508 + 0.203 + 0.002 + 8.637 
SStotal

#Total Sums of squares = 20.35
#Error Sums of squares = 8.637
#SSmodel = SStotal - SSerror

SSmodel = SStotal - 8.637
SSmodel
#The Model Sums of squares is 11.713

#Using summary.lm to get multiple R squared value (i.e. proportion of variance explained by model). This can also be done by dividing the SSmodel by SStotal
summary.lm(model1)
R2 = SSmodel/SStotal
R2
#The proportion of variance explained by the model is 0.5756

#Based on this model we can interpret the main effects of this 2-way ANOVA. Temperature is the only significant effect. Salamander larvae reared at high temperatures had higher growth rates. No need to do post-hoc testing as temperature only had two levels, so I know the difference is between those two levels.  

