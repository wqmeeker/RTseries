#------------------------------------------------------------------------------
#Example M1: International Airline Passengers Descriptive Graphics
#------------------------------------------------------------------------------

library(RTseries)
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
#illustrate different kinds of plots for the airline data
#

plot(Passengers.ts,main="International Airline Passengers 1949-1960",xlab="Year",ylab="Thousands of Passengers")
# -----------------------------------------------------------------------------------
# code for figure on page 1-11
plot(Passengers.ts,main="International Airline Passengers 1949-1960",xlab="Year",ylab="Thousands of Passengers",type="b",pch=0)
# -----------------------------------------------------------------------------------
# code for figure on page 1-12
plot(log(Passengers.ts),main="Log International Airline Passengers 1949-1960",xlab="Year",ylab="log(Thousands of Passengers)")
# -----------------------------------------------------------------------------------
# code for figure on page 1-13
plot(Passengers.ts,main="International Airline Passengers 1949-1960",xlab="Year",ylab="Thousands of Passengers",log="y")
# -----------------------------------------------------------------------------------

# code for figure on page 1-18
plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers",log="y")
f12 <- rep(1/12, 12)
y_lag <- filter(Passengers.ts, f12, sides=1)
lines(airline.regdat$Time, y_lag,lwd=2)
title(main="Smoothed International Airline Passengers 1949-1960 
      12-point moving average")
# -----------------------------------------------------------------------------------
# code for figure on page 1-19
plot(smooth(Passengers.tsd))
plot(smooth(Passengers.tsd),main="International Airline Passengers",xlab="Year",ylab="Thousands of Passengers")
# -----------------------------------------------------------------------------------
# code for figure on page 1-20
plot(Passengers.tsd-smooth(Passengers.ts),main="Passenger-smooth(Passenger)",xlab="Year",ylab="Thousands of Passengers")
# -----------------------------------------------------------------------------------
# code for figure on page 1-21
plot(Passengers.ts/smooth(Passengers.ts),main="Passenger/smooth(Passenger)",xlab="Year",ylab="ratio")
# -----------------------------------------------------------------------------------
# code for figure on page 1-24
plot(stl(Passengers.ts^ (0.25),s.window="periodic"))
# -----------------------------------------------------------------------------------
# code for figure on page 1-25
plot(stl(Passengers.ts^ (0.25),s.window=7))
# -----------------------------------------------------------------------------------
