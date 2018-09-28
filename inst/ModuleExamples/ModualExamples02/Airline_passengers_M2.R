#------------------------------------------------------------------------------
#Example M2: International Airline Passengers 
#------------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 2-3
#define Passengers as a "time series" object for simple plots and analyses
# read the data into a data frame
airline <- read.csv(RTseriesExtDataPath("airlineRegdat.csv"))
Passengers <- airline$Passengers
Passengers.ts <- ts(Passengers,freq=12,start=1949)
Passengers.tsd <- tsd(Passengers.ts,
                      data.title='International Airline Passengers',
                      response.units='Thousands of Passengers', time.units='Year')

#illustrate different kinds of plots for the airline data
#

plot(Passengers.ts,main="International Airline Passengers 1949-1960",xlab="Year",ylab="Thousands of Passengers")
# -----------------------------------------------------------------------------------
# code for figure on page 2-4
plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers",log="y",main="International Airline Passengers 1949-1960
     on Log Axis")
# -----------------------------------------------------------------------------------
# code for figure on page 2-5
#fit the linear trend nonseasonal model
airline.fit1 <- lm(log(Passengers.ts) ~ Time,data=airline)
summary(airline.fit1)

#plot data and the fitted line
plot(log(Passengers.tsd),main = "Log International Airline Passengers 1949-1960
     Model 1 Linear Regression Trend Line")
lines(airline$Time,predict(airline.fit1))
# -----------------------------------------------------------------------------------
# code for figure on page 2-6

plot(Passengers.tsd,log="y",main = "Log International Airline Passengers 1949-1960
     Model 1 Log Linear Regression Trend Line")
lines(airline$Time,exp(predict(airline.fit1)))

# -----------------------------------------------------------------------------------
# code for figure on page 2-12

plot(Passengers.tsd,log="y",main = "Log International Airline Passengers 1949-1960
     Model 1 Log Linear Regression Trend Line")
lines(airline$Time,exp(predict(airline.fit1)))
# -----------------------------------------------------------------------------------
# code for figure on page 2-25
#fit the linear trend seasonal model

airline.fit2 <- lm(log(Passengers) ~ Time + Month,data=airline)
summary(airline.fit2)


#the following is a little function that we need
# to make and plot the predictions

predict.time.series <- function(the.fit,data)
{
  #
  #function to compute predictions from a dummy-variable regression model
  #
  Month <-  data$Month
  Time <- data$Time
  #
  #look for log in call
  #
  #
  if(regexpr("log", as.character(the.fit$call)[2]) > 0) for(the.month in
                                                            sort(unique((as.character(Month))))) {
    lines(Time, exp(predict(the.fit, newdata =
                              data.frame(Time = Time, Month = rep(the.month,
                                                                  length = length(Time))))))
  }
  else for(the.month in sort(unique((as.character(Month))))) {
    print(the.month)
    lines(Time, (predict(the.fit, newdata = data.frame(
      Time = Time, Month = rep(the.month, length =
                                 length(Time))))))
  }
}

plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers",log="y")
predict.time.series(airline.fit2,data=airline)
text(1961.31, 719.785, "Jul",xpd=TRUE)
text(1961.33, 676.907, "Aug",xpd=TRUE)
text(1961.51, 417.322, "Nov",xpd=TRUE)
text(1961.33, 610.262, "Jun",xpd=TRUE)
text(1961.35, 527.431, "Mar",xpd=TRUE)

# -----------------------------------------------------------------------------------
# code for figure on page 2-26
plot(ts(residuals(airline.fit2)))

# -----------------------------------------------------------------------------------
# code for figure on page 2-27
plot(fitted.values(airline.fit2),residuals(airline.fit2))

# -----------------------------------------------------------------------------------
# code for figure on page 2-28
qqnorm(residuals(airline.fit2),type="p")

# -----------------------------------------------------------------------------------
# code for figure on page 2-29
acf(residuals(airline.fit2))

# -----------------------------------------------------------------------------------
# code for figure on page 2-33
#fit the linear trend seasonal model (using dummy variables with
#	different slopes)

airline.fit3 <- lm(log(Passengers) ~ Time + Month + Time:Month,data=airline)
summary(airline.fit3)

plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers",log="y")
predict.time.series(airline.fit3,data=airline)
text(1961.13, 739.388, "July",xpd=TRUE)
text(1961.24, 682.124, "Aug",xpd=TRUE)
text(1961.18, 423.779, "Nov",xpd=TRUE)
text(1961.15, 617.331, "Jun",xpd=TRUE)
text(1961.2, 558.692, "Mar",xpd=TRUE)

# -----------------------------------------------------------------------------------
# code for figure on page 2-38
#fit the linear trend seasonal model (using dummy variables with
#	common slopes)

airline.fit4 <- lm(Passengers ~ Time + Month + Time:Month,data=airline)
summary(airline.fit4)

plot(Passengers.ts,xlab="Year",ylab="Thousands of Passengers")
predict.time.series(airline.fit4,data=airline)

# -----------------------------------------------------------------------------------
# code for figure on page 2-39
plot(ts(residuals(airline.fit4)))

# -----------------------------------------------------------------------------------
# code for figure on page 2-40
plot(fitted.values(airline.fit4),residuals(airline.fit4))

# -----------------------------------------------------------------------------------
# code for figure on page 2-41
qqnorm(residuals(airline.fit4))

# -----------------------------------------------------------------------------------
# code for figure on page 2-42
acf(residuals(airline.fit4))





