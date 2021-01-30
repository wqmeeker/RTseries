FTindexReg<- read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/FinancialTimesIndex.csv")

#dim(FTindexReg)
#> dim(FTindexReg)
#[1] 51  4

# > FTindexReg[1:4,]
#  Observation FTindex FTCindex UKCarProd
#           1   148.5    96.21   121.874
#           2   165.4    93.74   126.260
#           3   178.5    91.37   145.248
#           4   187.3    86.31   160.370
#
# > FTindexReg[48:51,]
#    Observation FTindex FTCindex UKCarProd
# 48          48   349.3    85.95   434.255
# 49          49   359.7    90.73   475.890
# 50          50   320.0    92.42   439.365
# 51          51   299.9    87.18   413.666

FTindex.tsd <-  tsd(ts(FTindexReg$FTindex,frequency=4,start=c(1954,2)),data.title="Financial Times Index",response.units="Index",time.units="Year")

FTCindex.tsd <-  tsd(ts(FTindexReg$FTCindex,frequency=4,start=c(1952,3)),data.title="Financial Times Commodity Index (lagged 7 quarters)",response.units="Index",time.units="Year")

UKCarProd.tsd <-  tsd(ts(FTindexReg$UKCarProd,frequency=4,start=c(1952,4)),data.title="UK Automobile Production (lagged 6 quarters)",response.units="Thousands",time.units="Year")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postscript(file=Stat451FigurePath("FTindex.eps"))
plot(FTindex.tsd)
dev.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(FTindex.tsd)
iden(FTindex.tsd)
iden(FTindex.tsd, d=1)

plotCGKdata <-function(){
old.par <- par(mfrow=c(2, 2))
plot(FTindex.tsd, main="Financial Times Index")
plot(FTCindex.tsd, main="Financial Times Commodity Index (lagged 7 quarters)")
plot(UKCarProd.tsd, main="UK Automobile Production (lagged 6 quarters)")
par(old.par)
}

plotCGKdata()

# OLS
FTindexReg.out <- lm(FTindex ~ FTCindex + UKCarProd,data=FTindexReg)
summary(FTindexReg.out)


my.acf.plot(acf(residuals(FTindexReg.out)))

acf(residuals(FTindexReg.out))


# AR(1)
FTindex.model01 <- esti(FTindex.tsd, model=model.pdq(p=1))
# IMA(1,1)
FTindex.model02 <- esti(FTindex.tsd, model=model.pdq(q=1,d=1))
# random walk model
FTindex.model03 <- esti(FTindex.tsd, model=model.pdq(q=1,d=1), fixed=c(0))
# should be the same as above
FTindex.model04 <- esti(FTindex.tsd, model=model.pdq(d=1))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FTindex.model04 <- esti(FTindex.tsd, model=model.pdq(d=1),ps1=Stat451FigurePath("FTindexEstiM04P01.eps"),ps2=Stat451FigurePath("FTindexEstiM04P02.eps"),y.range=c(140,460))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# white noise (trivial) model
FTindex.model05 <- esti(FTindex.tsd, model=model.pdq())

# get the X matrix
FTindexXmat <- FTindexReg[,c(1,3,4)]

#we have the leading indicators out 10 quarters; fill with random-walk forecasts after that.
FTindexXmatXnew <- cbind(Observation=52:75,FTCindex=c(85.20,85.44,87.85,89.95,90.20,88.89,83.25,81.21,79.70,78.70,81.50,rep(81.50,13)),UKCarProd=c(399.160,449.564,437.555,426.616,399.254,334.587,367.997,393.808,375.968,381.692,rep(381.692,14)))

# gives same estimates as OLS, but skightly different standard errors; correlation in residuals
FTindex.model06 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew)[,c(2,3)], model=model.pdq())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FTindex.model06 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew)[,c(2,3)], model=model.pdq(),y.range=c(140,460))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# do not remove trend ---just use the IMA(1,1)
FTindex.model07 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew)[,c(2,3)], model=model.pdq(d=1,q=1))
acf(residuals(FTindex.model05))
1-pnorm(2.0078401) = 0.022

# do not remove trend ---juse use the difference
FTindex.model08 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat[,c(2,3)],FTindexXmatXnew[,c(2,3)]), model=model.pdq(d=1))
acf(residuals(FTindex.model05))
1-pnorm(2.0078401) = 0.022

# remove trend and use IMA(1,1)
FTindex.model09 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew), model=model.pdq(d=1,q=1))
acf(residuals(FTindex.model05))
1-pnorm(2.0078401) = 0.022

# remove trend  fix beta2 to be 0
FTindex.model10 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew), model=model.pdq(d=1,q=1),fixed=c(NA,NA,NA,0))

1-pchisq(430.1575 - 426.2804,1) = 0.0489


# just remove trend
FTindex.model11 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew), model=model.pdq())
acf(residuals(FTindex.model06))


# use Box-Newbold idea of differencing to remove trend
#  gives the same estimates as model09
# because no differenceing we get an intercept wither the same coef as "Observation" in model09#  compare with description in BK2011
#  but gives forecasts for the differences
# R did not want to difference the rbind result
FTindex.model12 <- esti(diff(FTindex.tsd), xreg=diff(as.matrix(rbind(FTindexXmat,FTindexXmatXnew)[,c(2,3)])), model=model.pdq(q=1))


# remove trend and use IMA(1,1)
FTindex.model13 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew), model=model.pdq(d=1))
acf(residuals(FTindex.model05))
1-pnorm(2.0078401) = 0.022


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FTindex.model13 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew), model=model.pdq(d=1),y.range=c(140,460))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
