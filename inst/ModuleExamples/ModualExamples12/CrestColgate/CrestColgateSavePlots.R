CrestColgate <- read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/CrestColgate.csv")


colgate.tsd <-  tsd(ts(CrestColgate$Colgate,frequency=52,start=c(1958,1)),data.title="Colgate Market Share",response.units="Percent",time.units="Year")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postscript(file=Stat451FigurePath("ColgateMarketShare.eps"))
plot(colgate.tsd,ylim=c(0.05,0.55),las=1)
dev.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




crest.tsd <-  tsd(ts(CrestColgate$Crest,frequency=52,start=c(1958,1)),data.title="Crest Market Share",response.units="Percent",time.units="year")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postscript(file=Stat451FigurePath("CrestMarketShare.eps"))
plot(crest.tsd,ylim=c(0.05,0.55),las=1)
dev.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postscript(file=Stat451FigurePath("CrestMarketShareIden.eps"))
iden(crest.tsd)
dev.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postscript(file=Stat451FigurePath("CrestMarketShareIdenDiff.eps"))
iden(crest.tsd, d=1)
dev.off()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crest.model01 <- esti(crest.tsd, model=model.pdq(d=1, q=1),ps1=Stat451FigurePath("CrestMarketShareEstiM01P01.eps"),ps2=Stat451FigurePath("CrestMarketShareEstiM01P02.eps"),y.range=c(0,0.6))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crest.model02 <- esti(crest.tsd, model=model.pdq(p=1, q=1),ps1=Stat451FigurePath("CrestMarketShareEstiM02P01.eps"),ps2=Stat451FigurePath("CrestMarketShareEstiM02P02.eps"),y.range=c(0,0.6))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crestPulse <- matrix(c(rep(0,134),1,rep(0,141+24)),ncol=1)

crestStep <- matrix(c(rep(0,134),1,rep(1,141+24)),ncol=1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crest.model03 <- esti(crest.tsd, model=model.pdq(d=1, q=1),xreg=crestStep,ps1=Stat451FigurePath("CrestMarketShareEstiM03P01.eps"),ps2=Stat451FigurePath("CrestMarketShareEstiM03P02.eps"),y.range=c(0,0.6))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crest.model04 <- esti(crest.tsd, model=model.pdq(p=1, q=1),xreg=crestStep,ps1=Stat451FigurePath("CrestMarketShareEstiM04P01.eps"),ps2=Stat451FigurePath("CrestMarketShareEstiM04P02.eps"),y.range=c(0,0.6))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

