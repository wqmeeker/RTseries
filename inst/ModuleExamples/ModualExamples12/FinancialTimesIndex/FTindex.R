
FTindexReg<- read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/FinancialTimesIndex.csv")

FTindex.ts <-  ts(FTindexReg$FTindex,frequency=4,start=c(1954,2))

FTindex.arima.out <- arima(FTindex.ts, order=c(0,1,1))
tsdiag(FTindex.arima.out)
acf(residuals(FTindex.arima.out))
qqnorm(residuals(FTindex.arima.out))
predict(FTindex.arima.out, n.ahead=24)

