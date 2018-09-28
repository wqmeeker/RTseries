#------------------------------------------------------------------------
#Example M3: sunspot data
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------

# code for figure on page 3-15
spot.ts <- ts(scan(RTseriesExtDataPath("sun_spot.txt")),start=1770)
plot(spot.ts)
title("Wolfer Sunspot Numbers 1770-1869",xlab="Year")

spot.tsd <- tsd(spot.ts,data.title="Wolfer Sunspots Numbers 1770-1869", time.units="Year", response.units="Number of Spots")
plot(spot.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 3-17
plot(spot.ts[-1],spot.ts[-100])
abline(lm(spot.ts[-1]~spot.ts[-100]))
#title("Wolfer Sunspot Numbers Correlation Between Observations\nSeparated by One Time Period")
text(25,125,paste("Correlation = ",format(cor(spot.ts[-1],spot.ts[-100]))))
text(31,135,paste("Autocorrelation = ",format(acf(spot.ts,plot=F)[[1]][2])))

# -----------------------------------------------------------------------------------
# code for figure on page 3-18
show.acf(spot.tsd)
# -----------------------------------------------------------------------------------
# code for figure on page 3-19
acf(spot.ts)
# -----------------------------------------------------------------------------------
# code for figure on page 3-21
plot(spot.ts)
# -----------------------------------------------------------------------------------
# code for figure on page 3-24
spotlag.frame <- as.data.frame(cbind(Spot=c(spot.ts,NA,NA,NA,NA),Spot1=c(NA,spot.ts,NA,NA,NA),Spot2=c(NA,NA,spot.ts,NA,NA),Spot3=c(NA,NA,NA,spot.ts,NA),Spot4=c(NA,NA,NA,NA,spot.ts)))
spot.ar1.fit <- lm(Spot ~ Spot1, data=spotlag.frame)
summary(spot.ar1.fit)
ts.diag.summary(spot.ar1.fit,data.tsd=spot.tsd)
# -----------------------------------------------------------------------------------
# code for figure on page 3-25
spot.ar2.fit <- lm(Spot ~ Spot1+Spot2, data=spotlag.frame)
summary(spot.ar2.fit)
ts.diag.summary(spot.ar2.fit,data.tsd=spot.tsd)
# -----------------------------------------------------------------------------------
# code for figure on page 3-26
spot.ar3.fit <- lm(Spot ~ Spot1+Spot2+Spot3, data=spotlag.frame)
summary(spot.ar3.fit)
ts.diag.summary(spot.ar3.fit,data.tsd=spot.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 3-27
spot.ar4.fit <- lm(Spot ~ Spot1+Spot2+Spot3+Spot4, data=spotlag.frame)
summary(spot.ar4.fit)
ts.diag.summary(spot.ar4.fit,data.tsd=spot.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 3-33
iden(spot.tsd)


