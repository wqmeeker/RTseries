
#------------------------------------------------------------------------
#ozone data, regression intervention model
#------------------------------------------------------------------------

#read in the complete ozone data, including dummy variables

ozone.xmat <- read.csv(RTseriesExtDataPath ("ozoneRegdat.csv"))

# need to strip off the "future" values from the response

ozone.tsd <- tsd(ts(ozone.xmat[1:216, 1], frequency=12, start=1955),
                 data.title="Monthly Average Ozone in Downtown Los Angeles",
                 response.units="pphm", time.units="Year")

plot(ozone.tsd)

#get the regression x matrix corresponding the the box-tiao
#intervention model
ozo.xreg <- cbind(ozone.xmat[,2], my.cumsum(ozone.xmat[,3],12),my.cumsum(ozone.xmat[,4],12))

iden(ozone.tsd)
iden(ozone.tsd, gamma=0)

iden(ozone.tsd, gamma=0, D=1)

#no regression
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9))
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9), gamma=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#regression models
#no transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9), xreg.in=ozo.xreg)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#log transformation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
esti(ozone.tsd, model=model.pdq(D=1,q=1,Q=1,period=12),	y.range=c(0.1,9), xreg.in=ozo.xreg, gamma=0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
