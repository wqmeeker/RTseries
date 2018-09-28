library("RTseries")
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# code for figure on page 12-26
FTI<-read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/FinancialTimesIndex.csv")
FTindex.ts <- ts(FTI$FTindex,freq=4,start=1954)
FTindex.tsd <- tsd(FTindex.ts, data.title = "Financial Times Index",response.units = "Index")
plot(FTindex.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 12-27
FTCindex.ts <- ts(FTI$FTCindex,freq=4,start=1952)
FTCindex.tsd <- tsd(FTCindex.ts, data.title = "Financial Times Commodity Index (lagged 7 quarters)",response.units = "Index")
plot(FTCindex.tsd)


UKCarProd.ts <- ts(FTI$UKCarProd,freq=4,start=1952)
UKCarProd.tsd <- tsd(UKCarProd.ts, data.title = "UK Automobile Production (lagged 6 quarters)",response.units = "Thousands")
plot(UKCarProd.tsd)

par(mfrow=c(2,2)) 
plot(FTindex.tsd)
plot(FTCindex.tsd)
plot(UKCarProd.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 12-29
FTindexReg.out<-lm(formula = FTindex ~ FTCindex + UKCarProd, data=FTI)
acf(residuals(FTindexReg.out))
# -----------------------------------------------------------------------------------
# code for figure on page 12-30 12-32
# get the X matrix
FTindexReg<- read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/FinancialTimesIndex.csv")

FTindexXmat <- FTindexReg[,c(1,3,4)]

#we have the leading indicators out 10 quarters; fill with random-walk forecasts after that.
FTindexXmatXnew <- cbind(Observation=52:75,FTCindex=c(85.20,85.44,87.85,89.95,90.20,88.89,83.25,81.21,79.70,78.70,81.50,rep(81.50,13)),UKCarProd=c(399.160,449.564,437.555,426.616,399.254,334.587,367.997,393.808,375.968,381.692,rep(381.692,14)))

# gives same estimates as OLS, but skightly different standard errors; correlation in residuals
FTindex.model06 <- esti(FTindex.tsd, xreg=rbind(FTindexXmat,FTindexXmatXnew)[,c(2,3)], model=model.pdq())


