#------------------------------------------------------------------------
#Example M10: ozone data, seasonal model
#------------------------------------------------------------------------

library("RTseries")

# -----------------------------------------------------------------------------------
# code for figure on page 10-3
ozone.tsd <- tsd(ts(scan(RTseriesExtDataPath("ozone.txt")),
                    frequency=12, start=1955),
                 data.title="Monthly Average Ozone in Downtown Los Angeles",
                 response.units="pphm", time.units="Year")
plot(ozone.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 10-5
plot(ozone.tsd)
abline(h=5,lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-6
plot(ozone.tsd,log="y")
abline(h=5,lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-7
iden(ozone.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 10-8
iden(ozone.tsd,gamma=0)

# -----------------------------------------------------------------------------------
# code for figure on page 10-9
iden(ozone.tsd,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-10
iden(ozone.tsd, D=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-11
iden(ozone.tsd, D=1,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-13 10-14 10-18
# model 1
esti(ozone.tsd, gamma=1, model=model.pdq(q=1, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 10-16 10-17 10-21
# model 2
esti(ozone.tsd, gamma=1, model=model.pdq(p=3, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 10-19 10-20 10-24
# model 3
esti(ozone.tsd, gamma=1, model=model.pdq(p=1,q=1, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 10-22 10-23 10-29 10-31 10-57
# model 4
esti(ozone.tsd, gamma=1, model=model.pdq(d=1,q=1, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 10-28 10-30 10-63
# model 8
esti(ozone.tsd, gamma=0, model=model.pdq(d=1,q=1, D=1, Q=1))

# code for figure on page 10-41
# Los Angeles Ozone Data Monthly Averages 1955-1972
plot(ozone.tsd)
abline(h=5,lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 10-43
plot(ozone.tsd)
abline(h=5,lwd=1)
abline(v=1960, lwd=1, lty=2)

# -----------------------------------------------------------------------------------
# code for figure on page 10-44
plot(ozone.tsd)
abline(h=5,lwd=1)
abline(v=c(1960,1966),lwd=1,lty=2)

# -----------------------------------------------------------------------------------
# code for figure on page 10-55 10-56 10-60 10-61
# model 9
esti(ozone.tsd, gamma=1, model=model.pdq(q=1, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 10-59 10-62
# model 10
esti(ozone.tsd, gamma=0, model=model.pdq(q=1, D=1, Q=1))







#------------------------------------------------------------------------
#ozone data, regression intervention model
#------------------------------------------------------------------------

#read in the complete ozone data, including dummy variables

ozone.xmat <- read.csv(RTseriesExtDataPath("ozoneRegdat.csv"))

#get the regression x matrix corresponding the the box-tiao
#intervention model
ozo.xreg <- cbind(ozone.xmat[,2], cumsum2(ozone.xmat[,3],12), cumsum2(ozone.xmat[,4],12))

#no transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9), xreg.in=ozo.xreg)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#log transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9), xreg.in=ozo.xreg, gamma=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
