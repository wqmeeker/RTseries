CrestColgate <- read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/CrestColgate.csv")


crest.tsd <-  tsd(ts(CrestColgate$Crest,frequency=52,start=c(1958,1)),data.title="Crest Market Share",response.units="Percent",time.units="Year")

plot(crest.tsd)
iden(crest.tsd)
iden(crest.tsd, d=1)

# IMA(1,1)
crest.model01 <- esti(crest.tsd, model=model.pdq(d=1, q=1), y.range=c(0, 0.6))

# ARMA(1,1)
crest.model02 <- esti(crest.tsd, model=model.pdq(p=1, q=1), y.range=c(0, 0.6))

#create the step function for the intervention models
crestStep <- matrix(c(rep(0,134), 1, rep(1,141+24)), ncol=1)

# IMA(1,1) with intervention term
crest.model03 <- esti(crest.tsd, model=model.pdq(d=1, q=1),xreg=crestStep, y.range=c(0, 0.6))

# ARMA(1,1) with intervention term
crest.model04 <- esti(crest.tsd, model=model.pdq(p=1, q=1),xreg=crestStep, y.range=c(0, 0.6))


#
# Could fit a similar model to the Colgate data to see their drop in market share
#
colgate.tsd <-  tsd(ts(CrestColgate$Colgate,frequency=52,start=c(1958,1)),data.title="Colgate Market Share",response.units="Percent",time.units="Week")

plot(colgate.tsd)

