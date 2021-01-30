Location: RTseries\inst\ModuleExamples\ExpectationVarianceCovariance\ExpectationVarianceCovariance.R

###
### install the latest version of RTseries
###
install.packages("https://wqmeeker.stat.iastate.edu/RTseries.tar.gz", repos=NULL)


Alternatively, to install RTseries from GitHub, use

install.packages("devtools")
library(devtools)
install_github("wqmeeker/RTseries")


library(RTseries)
color.test()


###
### we can also scan data directly via a URL
###
spot <- scan("https://wqmeeker.stat.iastate.edu/TimeseriesDatasets/spot.txt")
plot(spot ,type="l")

###
###Expectation (or process mean)
###

###
### Process: lightbulb life, testing 20 simultaneously
### Process parameters:  E(X)=1100, SD(X) = 100
###

mean(LightBulbLife(number=2000000, units="hours"))

LightBulbData <- LightBulbLife(number=200, units="hours")
plot(LightBulbData, xlab="Bulb Number", ylab="Light Bulb Life in Hours")
abline(h=mean(LightBulbData), lwd=3, col=4)


###
### other plots
###
hist(LightBulbData)
acf(LightBulbData)
qqnorm(LightBulbData)


###
###mean light bulb lifetime in terms of days?
###    Y = X/24
###  E(Y) = (1/24)*E(X)
###
###   E(Y) = (1/24)*1100 =  45.833
mean(LightBulbLife(number=2000000, units="days"))


###
###varaince and standard deviation of lightbulb life
###

plot.ts.deviations(LightBulbData)


var(LightBulbLife(number=2000000, units="hours"))
sd(LightBulbLife(number=2000000, units="hours"))
sqrt(var(LightBulbLife(number=2000000, units="hours")))
###
###   Var(Y) =  (1/24)^2*Var(X)
###   SD(Y) =  (1/24)*SD(X)
###   SD(Y) =  (1/24)*100 = 4.166667
###

sd(LightBulbLife(number=2000000, units="days"))



###
### furnase temperature observations every minute
### Process parameters: E(X) = 200 degrees C, SD(X) = 3 degrees C
###
mean(FurnaseTemperature(number=2000000, units="C"))
###
### Take a short realization
###
FurnaseData <- FurnaseTemperature(number=300, units="C")
plot(FurnaseData, xlab="Minutes")
abline(h=mean(FurnaseData), lwd=3, col=4)
hist(FurnaseData, xlab="Degrees C")
acf(FurnaseData)
meanFurnase <- mean(FurnaseData)


###
### mean temperature in degrees F
###
###   Degrees F =  32 + (9/5)C
###   E(F) =   32 + (9/5)E(C) = 32 + (9/5)*200 = 392
mean(FurnaseTemperature(number=2000000, units="F"))

###
### variance and standard deviation of temperature in degrees F
###
sd(FurnaseTemperature(number=2000000, units="C"))

###   Var(F) =  (9/5)^2Var(C) 
###   SD(F) =  (9/5)SD(C) 
###   SD(F) =  (9/5)*3 = 5.4

sd(FurnaseTemperature(number=2000000, units="F"))




###
### covariance/correlation
###
### scatter plot of first-order autocovariance
###
cbind(FurnaseData[-1], FurnaseData[-300])[1:10, ]

plot(FurnaseData[-1], FurnaseData[-300]) 

###
### scatter plot of first-order autocovariance now centered
###
plot(FurnaseData[-1] - meanFurnase, FurnaseData[-300] - meanFurnase) 
abline(h=0,lwd=2)
abline(v=0,lwd=2)


###
### Profit = Revenue - Expenses
###  E(Revenue) = 200, SD(Revenue) = 20, phi=0.0
###  E(Expenses) = 180, SD(Expenses) = 10, phi=0.0
###
###
### E(Profit) = E(Revenue) - E(Expenses)
###
Profit(number=20, units="dollars")
apply(Profit(number=2000000, units="dollars"), 2, mean)
ProfitData <- Profit(number=200, units="dollars")[,"Profit"]
plot(ProfitData)
abline(h=0, lwd=3, col=4)
acf(ProfitData)
pacf(ProfitData)


###  Var(Profit) = Var(Revenue) + Var(Expenses)
###  Sd(Profit) = sqrt(Var(Revenue) + Var(Expenses))
sqrt(20^2 + 10^2) = 22.36

apply(Profit(number=2000000, units="dollars"), 2, sd)

