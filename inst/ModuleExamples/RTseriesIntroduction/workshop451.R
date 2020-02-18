Location: \RTseries\inst\ModuleExamples\RTseriesIntroduction

If you have not yet instaled RTseries, you can use:

install.packages("http://www.public.iastate.edu/~wqmeeker/RTseries/RTseries.tar.gz", repos=NULL)

Then to load RTseries, use

library(RTseries)
color.test()





#------------------------------------------------------------------------------
#Example 1: Housing Start Data (built into RTSERIES)
#------------------------------------------------------------------------------
hstart
plot(hstart)

hstart.ts <- ts(hstart,frequency = 12, start = c(1966, 1))
hstart.ts
plot(hstart.ts)
plot(hstart.ts,xlab="Year",ylab="Thousands of Homes")
title(main="US Housing Starts 1966-1974")


hstart.tsd <- tsd(hstart.ts, data.title='US Housing Starts 1966-1974',
     response.units='Thousands of Homes', time.units='Year')
shaded.tsplot(hstart.tsd)
shaded.tsplot(hstart.tsd, top=TRUE)



#------------------------------------------------------------------------------
#Example 2: International Airline Passengers Descriptive Graphics
#------------------------------------------------------------------------------

# read the data into a data frame
airline <- read.csv(RTseriesExtDataPath("airlineRegdat.csv"))

#print the first 10 rows of the frame
airline[1:10,]

#define Passengers as a "time series" object for simple plots and analyses
Passengers <- airline$Passengers
Passengers.ts <- ts(Passengers,freq=12,start=1949)
Passengers.tsd <- tsd(Passengers.ts,
                      data.title='International Airline Passengers',
     response.units='Thousands of Passengers', time.units='Year')


#
#illustrate different kinds of plots for the airline data
#

plot(Passengers.tsd)

plot(Passengers.tsd, type="b", pch=0)

plot(log(Passengers.tsd), ylab="log(Thousands of Passengers)")

plot(Passengers.tsd, log="y")

plot(smooth(Passengers.tsd))
plot(smooth(Passengers.tsd),main="International Airline Passengers",xlab="Year",ylab="Thousands of Passengers")

plot(Passengers.tsd-smooth(Passengers.ts),main="Passenger-smooth(Passenger)",xlab="Year",ylab="Thousands of Passengers")

plot(Passengers.ts/smooth(Passengers.ts),main="Passenger/smooth(Passenger)",xlab="Year",ylab="ratio")

plot(log(Passengers.ts/smooth(Passengers.ts)),main="log(Passenger/smooth(Passenger))",xlab="Year",ylab="log ratio")

#------------------------------------------------------------------------------
#Example x: International Airline Passengers: Regression Models
#------------------------------------------------------------------------------

#fit the linear trend nonseasonal model 
airline.fit1 <- lm(log(Passengers) ~ Time, data=airline)
summary(airline.fit1)


#plot data and the fitted line

plot(log(Passengers.tsd))
lines(airline$Time,predict(airline.fit1))

# plot data and the fitted line, but use a log axis; 
# need to plot antilogs of predictions

plot(Passengers.tsd,log="y")
lines(airline$Time,exp(predict(airline.fit1)))


#fit the linear trend seasonal model

airline.fit2 <- lm(log(Passengers) ~ Time + Month, data=airline)
summary(airline.fit2)

plot(Passengers.ts, xlab="Year", ylab="Thousands of Passengers", log="y")
seasonal.predictions(airline.fit2, the.data.frame=airline)
text(1961.31, 719.785, "Jul",xpd=TRUE)
text(1961.33, 676.907, "Aug",xpd=TRUE)
text(1961.51, 417.322, "Nov",xpd=TRUE)
text(1961.33, 610.262, "Jun",xpd=TRUE)
text(1961.35, 527.431, "Mar",xpd=TRUE)

plot(ts(residuals(airline.fit2)))
plot(fitted.values(airline.fit2), residuals(airline.fit2))
qqnorm(residuals(airline.fit2))
acf(residuals(airline.fit2))

anova(airline.fit1)
anova(airline.fit2)
anova(airline.fit1,airline.fit2)


#fit the linear trend seasonal model (using dummy variables with
#	different slopes)

airline.fit3 <- lm(log(Passengers) ~ Time + Month + Time:Month,data=airline)
summary(airline.fit3)

plot(Passengers.ts, xlab="Year", ylab="Thousands of Passengers", log="y")
seasonal.predictions(airline.fit3, the.data.frame=airline)
text(1961.13, 739.388, "July",xpd=TRUE)
text(1961.24, 682.124, "Aug",xpd=TRUE)
text(1961.18, 423.779, "Nov",xpd=TRUE)
text(1961.15, 617.331, "Jun",xpd=TRUE)
text(1961.2, 558.692, "Mar",xpd=TRUE)


plot(ts(residuals(airline.fit3)))
plot(fitted.values(airline.fit3), residuals(airline.fit3))
qqnorm(residuals(airline.fit3))
acf(residuals(airline.fit3))

anova(airline.fit2)
anova(airline.fit3)
anova(airline.fit2,airline.fit3)

#fit the linear trend seasonal model (using dummy variables with
#	common slopes)

airline.fit4 <- lm(Passengers ~ Time + Month + Time:Month,data=airline)
summary(airline.fit4)

plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers")
seasonal.predictions(airline.fit4, the.data.frame=airline)

plot(ts(residuals(airline.fit4)))
plot(fitted.values(airline.fit4), residuals(airline.fit4))
qqnorm(residuals(airline.fit4))
acf(residuals(airline.fit4))



#------------------------------------------------------------------------------
#Example 3: Best Buy Quarterly Sales
#------------------------------------------------------------------------------


BestBuySales <- read.csv(RTseriesExtDataPath("BestBuySalesRegdat.csv"))
BestBuySales.tsd <- tsd(ts(BestBuySales$MillionsOfDollars, frequency=4, start=2000), data.title='Best Buy Quarterly Sales 2000-2012',
     response.units='Millions of Dollars', time.units='Year')

BestBuySales.fit2 <- lm(MillionsOfDollars ~ Time + Quarter, data=BestBuySales)
summary(BestBuySales.fit2)

plot(BestBuySales.tsd, xlab="Year", ylab="Thousands of Passengers")
seasonal.predictions(BestBuySales.fit2, the.data.frame=BestBuySales)



BestBuySales.fit3 <- lm(log(MillionsOfDollars) ~ Time + Quarter, data=BestBuySales)
summary(BestBuySales.fit3)

plot(BestBuySales.tsd, xlab="Year", ylab="Thousands of Passengers", log="y")
seasonal.predictions(BestBuySales.fit3, the.data.frame=BestBuySales)



#------------------------------------------------------------------------
#example probability plot simulations
#------------------------------------------------------------------------
#Compare the results of the following. Try each several times.


multiple.probplot.sim()
multiple.probplot.sim(distribution="exponential")
multiple.probplot.sim(distribution="weibull", parameter=0.8)
multiple.probplot.sim(sample.size.vec=c(25, 50, 100))


#------------------------------------------------------------------------
#example change in IGA common stock price
#------------------------------------------------------------------------



IGA.stock <- read.csv(RTseriesExtDataPath("INGA_Amsterdam.csv"))
IGA.stock.tsd <- tsd(ts(IGA.stock$Closing), time.units="Days", data.title="IGA Stock Price 2010", response.units="Euros")
plot(IGA.stock.tsd)

change.IGA.stock.ts <- ts(scan(RTseriesExtDataPath("INGA_AmsterdamDiff.txt")), start=1, freq=1)
change.IGA.stock.tsd <- tsd(change.IGA.stock.ts, time.units="Days", data.title="Change in IGA Stock Price 2010", response.units="Euros")
plot(change.IGA.stock.tsd)
abline(h = mean(change.IGA.stock.tsd), lwd = 2, col = 6)
abline(h=mean(change.IGA.stock.tsd)-2*sqrt(var(change.IGA.stock.tsd)), col=6, lty=6, lwd=2)
abline(h=mean(change.IGA.stock.tsd)+2*sqrt(var(change.IGA.stock.tsd)), col=6, lty=6, lwd=2)

plot(change.IGA.stock.tsd[-1], change.IGA.stock.tsd[-257])
cor(change.IGA.stock.tsd[-1], change.IGA.stock.tsd[-257])
text(12,-5,paste("Correlation=",cor(change.IGA.stock.tsd[-1], change.IGA.stock.tsd[-257])))


length(change.IGA.stock.tsd)
mean(change.IGA.stock.tsd)
var(change.IGA.stock.tsd)
sqrt(var(change.IGA.stock.tsd))
sqrt(var(change.IGA.stock.tsd)/257)
mean(change.IGA.stock.tsd) - qt(.975,257-1)*sqrt(var(change.IGA.stock.tsd)/257)
mean(change.IGA.stock.tsd) + qt(.975,257-1)*sqrt(var(change.IGA.stock.tsd)/257)
mean(change.IGA.stock.tsd) - qt(.975,257-1)*sqrt(var(change.IGA.stock.tsd)*(1+1/257))
mean(change.IGA.stock.tsd) + qt(.975,257-1)*sqrt(var(change.IGA.stock.tsd)*(1+1/257))






#------------------------------------------------------------------------
#example change in inventory data
#    Appeared as Case 1 in Pankrantz (1983)
#------------------------------------------------------------------------


change.inventory.ts <- ts(scan(RTseriesExtDataPath("ChangeInventory.txt")), start=1955, freq=4)

change.inventory.tsd <- tsd(change.inventory.ts, time.units="Year", data.title="Change in Inventories", response.units="Billions of Dollars")
plot(change.inventory.tsd)
abline(h = mean(change.inventory.tsd), lwd = 2, col = 6)
abline(h=mean(change.inventory.tsd)-2*sqrt(var(change.inventory.tsd)), col=6, lty=6, lwd=2)
abline(h=mean(change.inventory.tsd)+2*sqrt(var(change.inventory.tsd)), col=6, lty=6, lwd=2)
plot(change.inventory.tsd[-1], change.inventory.tsd[-60])
cor(change.inventory.tsd[-1], change.inventory.tsd[-60])
text(12, -5, paste("Correlation=", round(cor(change.inventory.tsd[-1], change.inventory.tsd[-60]), digits=3)))


#------------------------------------------------------------------------
#some other things
#------------------------------------------------------------------------
show.true.acfpacf(model=list(ar=.9))
multiple.acf.sim(model=list(ar=c(.9)))

arma.roots(c(.5,-.9,.1,.5))

iden(Passengers.tsd)

iden(Passengers.tsd,d=1,D=1)
iden(Passengers.tsd,d=1,D=1,gamma=0)

#------------------------------------------------------------------------
#example 
#example case 1 of pankratz
#------------------------------------------------------------------------

savings.rate.ts <- ts(scan(RTseriesExtDataPath("SavingsRate.txt")),start=1955,frequency=4)
savings.rate.tsd <- tsd(savings.rate.ts,data.title="US Savings Rate for 1955-1980", time.units="Year", response.units="Percent of Disposable Income")
savings.rate.tsd
plot(savings.rate.tsd)

mean(savings.rate.tsd)

var(savings.rate.tsd)

sqrt(var(savings.rate.tsd))

abline(h=mean(savings.rate.tsd))


#------------------------------------------------------------------------
#example oct 1975 nyse composite index
#------------------------------------------------------------------------
#read in the data
nyseci.tsd <- tsd(ts(scan(RTseriesExtDataPath("nyseci1.txt"))), data.title="NYSE Composite Index October 1975",
     response.units='Change in Value', time.units='Day')
plot(nyseci.tsd)
#use the diff function to get the day-to-day changes
nysecidiff <- diff(nyseci.tsd)
plot(nysecidiff)



#------------------------------------------------------------------------
#sunspot data
#------------------------------------------------------------------------


spot.ts <- ts(scan(RTseriesExtDataPath("sun_spot.txt")),start=1770)

spot.tsd <- tsd(spot.ts,data.title="Wolfer Sunspots 1770-1869", time.units="Year", response.units="Number of Spots")
plot(spot.tsd)

plot(spot.tsd[-1],spot.tsd[-100])
show.acf(spot.tsd)
acf(spot.tsd, main="")
title("Wolfer Sunspot Data 1770-1869")

spotlag.frame <- as.data.frame(cbind(Spot=c(spot.tsd,NA,NA,NA,NA),Spot1=c(NA,spot.tsd,NA,NA,NA),Spot2=c(NA,NA,spot.tsd,NA,NA),Spot3=c(NA,NA,NA,spot.tsd,NA),Spot4=c(NA,NA,NA,NA,spot.tsd)))

spot.ar1.fit <- lm(Spot ~ Spot1, data=spotlag.frame)
summary(spot.ar1.fit)
ts.diag.summary(spot.ar1.fit, data.tsd=spot.tsd)


#do it all with a simpler summary
ts.diag.summary(spot.ar1.fit,data.tsd=spot.tsd)

spot.ar2.fit <- lm(Spot ~ Spot1 + Spot2, data=spotlag.frame)
summary(spot.ar2.fit)
ts.diag.summary(spot.ar2.fit, data.tsd=spot.tsd)

spot.ar3.fit <- lm(Spot ~ Spot1 + Spot2 + Spot3, data=spotlag.frame)
summary(spot.ar3.fit)
ts.diag.summary(spot.ar3.fit, data.tsd=spot.tsd)


spot.ar4.fit <- lm(Spot ~ Spot1 + Spot2 + Spot3 + Spot4, data=spotlag.frame)
summary(spot.ar4.fit)
ts.diag.summary(spot.ar4.fit, data.tsd=spot.tsd)


iden(spot.tsd)

esti(spot.tsd,model=model.pdq(p=1),gamma=0.50)

esti(spot.tsd,model=model.pdq(p=2),gamma=0.50)
esti(spot.tsd,model=model.pdq(p=2),gamma=1)
esti(spot.tsd,model=model.pdq(p=3),gamma=0.50, print.table = TRUE)
esti(spot.tsd,model=model.pdq(p=16),gamma=0.50, print.table = TRUE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(spot.tsd[-1],spot.tsd[-100])
abline(lm(spot.tsd[-1]~spot.tsd[-100]))
#title("Wolfer Sunspot Numbers Correlation Between Observations\nSeparated by One Time Period")
text(25,125,paste("Correlation = ", round(cor(spot.tsd[-1],spot.tsd[-100]), 3)))
text(31,135,paste("Autocorrelation = ", round(acf(spot.tsd,plot=F)[[1]][2], 3)))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.acf(spot.tsd)

acf(spot.tsd)
iden(spot.tsd)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(change.inventory.tsd)

abline(h = mean(change.inventory.ts), lwd = 2, col = 6)
abline(h=mean(change.inventory.ts)-2*sqrt(var(change.inventory.ts)),col=6,lty=6,lwd=2)
abline(h=mean(change.inventory.ts)+2*sqrt(var(change.inventory.ts)),col=6,lty=6,lwd=2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#R commands for arma1 handout
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mfrow=c(2,1))
plot(change.inventory.tsd)
abline(h=mean(change.inventory.ts))
plot(change.inventory.tsd-mean(change.inventory.tsd))
abline(h=0)
par(mfrow=c(1,1))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ma=.95))
show.true.acfpacf(model=list(ma=-.95))
iden.sim(model=list(ar=c(0),ma=c(.95)),realization.size=75)
iden.sim(model=list(ar=c(0),ma=c(.95)),realization.size=300)
iden.sim(model=list(ar=c(0),ma=c(-.95)),realization.size=75)
iden.sim(model=list(ar=c(0),ma=c(-.95)),realization.size=300)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arma.roots(c(1.5,.4))
arma.roots(c(.5,-.9))
arma.roots(c(.5,-.9,.1,.5))
arma.roots(c(0.8))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ma=c(.78,.2)))
show.true.acfpacf(model=list(ma=c(1,-.95)))
iden.sim(model=list(ma=c(.78, 0.2)),realization.size=75)
iden.sim(model=list(ma=c(.78, 0.2)),realization.size=300)
iden.sim(model=list(ma=c(1, -0.95)),realization.size=75)
iden.sim(model=list(ma=c(1, -0.95)),realization.size=300)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#R commands for arma2 handout
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arvar.tsd <- iden.sim(model=list(ar=c(0.90)), realization.size=275, plot=FALSE)
plot(arvar.tsd,type="n")
lines(arvar.tsd[-seq(225:275)], type="b")
title(xlab="Time")
abline(h=mean(arvar.tsd), lwd=3)
abline(h=mean(arvar.tsd) + 2*sqrt(var(arvar.tsd)), lty=3, lwd=2)
abline(h=mean(arvar.tsd) - 2*sqrt(var(arvar.tsd)), lty=3, lwd=2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ar=0.95))
show.true.acfpacf(model=list(ar=0.99999))
show.true.acfpacf(model=list(ar=-0.95))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iden.sim(model=list(ar=c(0.95)),realization.size=75)
iden.sim(model=list(ar=c(0.95)),realization.size=300)
iden.sim(model=list(ar=c(0.99999)),realization.size=75)
iden.sim(model=list(ar=c(0.99999)),realization.size=300)
iden.sim(model=list(ar=c(-0.95)),realization.size=75)
iden.sim(model=list(ar=c(-0.95)),realization.size=300)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ar=c(0.78, 0.2)))
show.true.acfpacf(model=list(ar=c(1, -0.95)))
iden.sim(model=list(ar=c(.78, 0.2)), realization.size=75)
iden.sim(model=list(ar=c(0.78, 0.2)), realization.size=300)
iden.sim(model=list(ar=c(1, -0.95)), realization.size=75)
iden.sim(model=list(ar=c(1, -0.95)), realization.size=300)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ar=0.95, ma = -0.95))
iden.sim(model=list(ar=0.95, ma = -0.95), realization.size=75)
show.true.acfpacf(model=list(ar=-0.95, ma = 0.95))
iden.sim(model=list(ar=-0.95, ma = 0.95), realization.size=75)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show.true.acfpacf(model=list(ar=0.9, ma = 0.5))
iden.sim(model=list(ar=0.9, ma = 0.5), realization.size=75)
show.true.acfpacf(model=list(ar=-0.9, ma = -0.5))
iden.sim(model=list(ar=-0.9, ma = -0.5), realization.size=75)
show.true.acfpacf(model=list(ar=0.9, ma = 0.9))
iden.sim(model=list(ar=0.90001, ma = 0.9), realization.size=75)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iden(simsta05.tsd)
iden(simsta06.tsd)
iden(simsta07.tsd)
iden(simsta08.tsd)
iden(simsta11.tsd)
iden(simsta13.tsd)
iden(simsta15.tsd)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#commands for the nonstationary handout
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(Passengers.tsd, xlab="Year", ylab="Thousands of Passengers")
iden(Passengers.tsd)
iden(Passengers.tsd, gamma=0)
iden.sim(model=list(ar=-0.9, ma = -0.5), realization.size=75)
show.true.acfpacf(model=list(ar=0.9, ma = 0.9))
iden.sim(model=list(ar=0.90001, ma = 0.9), realization.size=75)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#commands for the nonstat handout
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(Passengers.tsd)
iden(Passengers.tsd,gamma=0.5)
iden(Passengers.tsd,gamma=-0.3333)
iden(Passengers.tsd,gamma=-1)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mfrow=c(1,2))
plot(c(1,2,3,4,5,6,7),c(2,4,8,16,32,64,128),type="b",xlab="Time",ylab="Number")
title(main= "100% Growth per Period")
plot(c(1,2,3,4,5,6,7),log(c(2,4,8,16,32,64,128)),
	type="b",xlab="Time",ylab="Log Number")
title(main= "Log of 100% Growth per Period")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mfrow=c(2,1))
plot(c(1,2,3,4,5,6,7,8,9,10,11),c(100,110,108,115,119,118,125,127,125,134,140),
	type="b",xlab="Time",ylab="Dollars")
title(main= "Cumulative Winnings")
plot(c(2,3,4,5,6,7,8,9,10,11),c(10,-2,7,4,-1,7,2,-2,9,6),
	type="b",xlab="Time",ylab="Dollars")
title(main= "Incremental Winnings")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#example case 6 of pankratz
att.stock.tsd <- tsd(ts(scan(RTseriesExtDataPath("att_stock.txt")), frequency=1, 	start=1),data.title="1979 Weekly Closing Price of ATT Common Stock",time.units="Week", response.units="Dollars")
iden(att.stock.tsd)
iden(att.stock.tsd,d=1)

iden(simnsa.tsd)
iden(simnsa.tsd,d=1)

iden(simnsb.tsd)
iden(simnsb.tsd,d=1)

iden(simnsc.tsd)
iden(simnsc.tsd,d=1)

iden(simnsd.tsd)
iden(simnsd.tsd,d=1)

iden(simnse.tsd)
iden(simnse.tsd,d=1)
iden(simnse.tsd,d=2)

iden(simnsf.tsd)
iden(simnsf.tsd,d=1)
iden(simnsf.tsd,d=2)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(pty="s",mar=c(6,6,4,2))
plot(c(-1,1), c(-1,1), type="n", xlab=bquote(phi[1]), ylab=bquote(theta[2]), cex=2, cex.lab=2, xaxs="i", yaxs="i", las=1)
abline(v=1,lwd=10)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mar=c(6,6,4,2))
plot(c(-2,2),c(-1,1),type="n",xlab=bquote(phi[1]),
	ylab=bquote(phi[2]), cex=2, cex.lab=2, xaxs="i", yaxs="i", las=1)
abline(a=1, b=-1, lwd=10)
abline(a=1, b=1)
abline(a=0, b=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#simple example for Project 1

#example case 1 of pankratz
savings.rate.tsd <- tsd(ts(scan(RTseriesExtDataPath("SavingsRate.txt")), frequency=4,start=1955),data.title="Change in Inventory", response.units="Billions of Dollars",time.units="Year")

plot(savings.rate.tsd)
abline(h=mean(savings.rate.tsd),lwd=2,col=6)
abline(h=mean(savings.rate.tsd)-2*sqrt(var(savings.rate.tsd)),
	col=6,lty=6,lwd=2)
abline(h=mean(savings.rate.tsd)+2*sqrt(var(savings.rate.tsd)),
	col=6,lty=6,lwd=2)

iden(savings.rate.tsd)
esti(savings.rate.tsd,model=model.pdq(p=1))
esti(savings.rate.tsd,model=model.pdq(p=1,q=1), print.table = TRUE)
esti(savings.rate.tsd,model=model.pdq(p=1,q=2))

iden(savings.rate.tsd,d=1)
esti(savings.rate.tsd,model=model.pdq(d=1,q=1))
esti(savings.rate.tsd,model=model.pdq(d=1,q=2))

#
#------------------------------------------------------------------------
#device inventory
#------------------------------------------------------------------------

device.inventory.tsd <-  tsd(ts(scan(RTseriesExtDataPath("device_inventory.txt")), frequency=12,start=1984),data.title="Device Inventory 1984-1994", response.units="Hundreds of Devices",time.units="Year")



#------------------------------------------------------------------------
#ozone data, seasonal model
#------------------------------------------------------------------------

ozone.tsd <- tsd(ts(scan(RTseriesExtDataPath("ozone.txt")),
                    frequency=12, start=1955),
                 data.title="Monthly Average Ozone in Downtown Los Angeles",
                 response.units="pphm", time.units="Year")

#pphm is parts per hundred million

#identification
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(ozone.tsd)
abline(h=mean(ozone.tsd), lwd=3)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iden(ozone.tsd)
iden(ozone.tsd, D=1)
iden(ozone.tsd, d=1)
iden(ozone.tsd, d=1, D=1)

#
#estimation
#
#no transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),y.range=c(0.1,9))
esti(ozone.tsd, model=model.pdq(D=1,p=3,Q=1,period=12),y.range=c(0.1,9))
esti(ozone.tsd, model=model.pdq(D=1,p=1,q=1,Q=1,period=12),y.range=c(0.1,9))
esti(ozone.tsd, model=airline.model,y.range=c(0.1,9))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#log transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9),gamma=0)
esti(ozone.tsd,model=model.pdq(D=1,p=3,Q=1,period=12),	y.range=c(0.1,9),gamma=0)
esti(ozone.tsd, model=model.pdq(D=1,p=1,q=1,Q=1,period=12), y.range=c(0.1,9),gamma=0)
esti(ozone.tsd, model=airline.model,y.range=c(0.1,9),gamma=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#------------------------------------------------------------------------
#ozone data, regression intervention model
#------------------------------------------------------------------------

#read in the complete ozone data, including dummy variables
ozoneRegdat <- read.csv(RTseriesExtDataPath("ozoneRegdat.csv"))
ozoneRegdat[1:10,]
#get the regression x matrix corresponding the the box-tiao
#intervention model
ozo.xreg <- cbind(ozoneRegdat[,2], ts.cumsum(ozoneRegdat[,3], lag=12), ts.cumsum(ozoneRegdat[,4], lag=12))

#no transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1, q=1, Q=1, period=12), y.range=c(0.1, 9), xreg.in=ozo.xreg)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#log transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1, q=1, Q=1, period=12), y.range=c(0.1, 9), xreg.in=ozo.xreg, gamma=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#------------------------------------------------------------------------------
#Example x: Gas Rate illustration of ACF
#------------------------------------------------------------------------------
#
# plot the data
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(gasrx.tsd)
abline(h=mean(gasrx.tsd))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(gasry.tsd)
abline(h=mean(gasry.tsd))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Identification of models for both input and output time series
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iden(gasrx.tsd)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iden(gasry.tsd)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# Univariate estimation for both input and output time series
#
esti(gasrx.tsd, model=model.pdq(p=3), y.range=c(-4.5, 4.5))
gasrx.ar4.out <- esti(gasrx.tsd, model=model.pdq(p=4), y.range=c(-4.5, 4.5))
esti(gasrx.tsd, model=model.pdq(p=5), y.range=c(-4.5, 4.5))
#ar(4) looks good for the input

esti(gasry.tsd, model=model.pdq(p=2), y.range=c(45,65))
esti(gasry.tsd, model=model.pdq(p=3), y.range=c(45,65))
esti(gasry.tsd, model=model.pdq(p=4), y.range=c(45,65))
gasry.ar5.out <- esti(gasry.tsd, model=model.pdq(p=5), y.range=c(45,65))
#ar(4) or ar(5) looks good for the output

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#models and parameter estimates for prewhitening
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gasrx.model <- model.pdq(p=4)
gasry.model <- model.pdq(p=5)
gasrx.coefficients <- coefficients(gasrx.ar4.out)
gasry.coefficients <- coefficients(gasry.ar5.out)
null.model <- model.pdq()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# cross correlation of the raw input and output
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ccf(gasry.tsd, gasrx.tsd, ylab="CCF", ylim=c(-1,1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# cross correlation between the residuals of the input and output models
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prewhiten(gasry.tsd, gasrx.tsd, x.model=gasrx.model, x.coefficients=gasrx.coefficients, y.model = gasry.model, y.coefficients = gasry.coefficients)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# cross correlation using the input model to filter both input and output time series
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prewhiten(gasry.tsd, gasrx.tsd, x.model=gasrx.model, x.coefficients=gasrx.coefficients)


#
# generate a matrix of time series that have leading indicators in them
# first add NA's at the beginning (for proper lagging) and 0's at the end of the gas rate time series for forecasting
#

#
lead.matrix(gasrx.tsd, lagvec=c(3,4,5,6),number.forecasts=30)[c(1:8,295:310),]
#
#now estimate the transfer function models.
#    need to specify the future x time series
#
#note that the forecasts from function esti (actually arima)
#       do not account for the uncertainty in the
# 	future x values (assumed to be 0 in the example)


#
# first look back 6 time periods
#
est.out <- esti(gasry.tsd, model=model.pdq(p=3), xreg.in=lead.matrix(gasrx.tsd, lagvec=c(3, 4, 5, 6)), y.range=c(45,65))

#
# now look back 7 time periods
#
est.out <- esti(gasry.tsd, model=model.pdq(p=3), xreg.in=lead.matrix(gasrx.tsd, lagvec=c(3, 4, 5, 6, 7)), y.range=c(45,65))

#
# now look back 8 time periods and go to AR(2)
#
est.out <- esti(gasry.tsd, model=model.pdq(p=2), xreg.in=lead.matrix(gasrx.tsd, lagvec=c(3, 4, 5, 6, 7, 8)), y.range=c(45,65))


show.true.acfpacf(model=list(ma=c(.9)))
show.true.acfpacf(model=list(ma=c(.5)))
show.true.acfpacf(model=list(ma=c(-.5)))
show.true.acfpacf(model=list(ma=c(-.5,-.75)))
show.true.acfpacf(model=list(ma=c(.9,.81,.73)))

multiple.acf.sim(model=list(ma=c(.9)))
multiple.acf.sim(model=list(ma=c(.5))) 
multiple.acf.sim(model=list(ma=c(-.5)))
multiple.acf.sim(model=list(ma=c(-.5,-.75)))
multiple.acf.sim(model=list(ma=c(.9,.81,.73))) 



arma.roots(c(1.6,.4))
arma.roots(c(0,.3))
arma.roots(c(.4,.9))
