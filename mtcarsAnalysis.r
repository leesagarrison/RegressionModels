setwd("C:\\leesa\\training\\coursera\\RegressionModels")

data(mtcars)
pairs(mtcars, panel=panel.smooth, main="mtcars data", col=3)

LGcor<-cor(mtcars)
sortabs((LGcor[,1]))
cor(mtcars)

fitT <- lm(mpg~am, data=mtcars)
summary(fitT)
aggregate(mpg~am, data=mtcars, mean)
am      mpg
#1  0 17.14737
#2  1 24.39231
#looking only at transmission type you would expect that cars with manual 
#transmissions would get 7.245 more mpg than those with an automatic transmission


summary(lm(mpg~.,data=mtcars))
summary(lm(mpg~wt, data=mtcars))$coefficients
summary(lm(mpg~am+wt, data=mtcars))$coefficients
summary(lm(mpg~am+wt+cyl, data=mtcars))$coefficients

par(mfrow = c(2, 2))
#plot relationship between mpg and weight (using tranmsition type as factor)

xyplot(mpg~wt|factor(am), data=mtcars, type=c("p", "r"))
#plot relationship between mpg and weight (using tranmsition type as factor)
xyplot(mpg~wt|factor(cyl), data=mtcars, type=c("p", "r"))

fitT <- lm(mpg~am, data=mtcars)


fitWCD<-lm(mpg~wt+cyl+disp, data=mtcars)
vif(fitWCD)

fitWC<-lm(mpg~wt+cyl, data=mtcars)
summary(fitWC)

fitTWC<-lm(mpg~am+wt+cyl, data=mtcars)
summary(fitTWC)


#confidence intervals

confint(lm(mpg~wt+cyl, data=mtcars))

par(mfrow = c(2, 2))
plot(fitTWC)
plot(predict(fitTWC), resid(fitTWC), pch = '.')

library(car)
#variance inflation factor
vif(fitTWC)  #2-5 lecture
#am       wt      cyl 
#1.924955 3.609011 2.584066 
anova(fitTWC, fitWC)
#Analysis of Variance Table

#Model 1: mpg ~ wt + cyl
#Model 2: mpg ~ am + wt + cyl
#Res.Df    RSS Df Sum of Sq      F Pr(>F)
#1     29 191.17                           
#2     28 191.05  1   0.12491 0.0183 0.8933


plot(fitted(fitWC), resid(fitWC), main="Exhibit XX \n Resdiual Plot (MPG~Weight+Cylinders)", 
     +      xlab="Fitted", ylab="Residuals")
abline(h=0, lty=2)

