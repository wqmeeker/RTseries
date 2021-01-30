library("RTseries")
gasrxy<-read.csv(RTseriesExtDataPath("gasrxy.csv"))
gasr.xymat <- as.matrix(gasrxy, ncol=2,byrow=T) 
# -----------------------------------------------------------------------------------
# code for figure on page 11-4
plot(gasrx.tsd)
abline(h=mean(gasrx.tsd))

# -----------------------------------------------------------------------------------
# code for figure on page 11-5
plot(gasry.tsd)
abline(h=mean(gasry.tsd))

# -----------------------------------------------------------------------------------
# code for figure on page 11-6
iden(gasrx.tsd)
# -----------------------------------------------------------------------------------
# code for figure on page 11-7
iden(gasry.tsd)
# -----------------------------------------------------------------------------------
# code for figure on page 11-15 11-16 11-36
esti(gasry.tsd, model=model.pdq(p=5),y.range=c(45,65))

# -----------------------------------------------------------------------------------
# code for figure on page 11-17 11-18
esti(gasrx.tsd, model=model.pdq(p=4),y.range=c(-4.5,4.5))

# -----------------------------------------------------------------------------------
# code for figure on page 11-23
#??
#models for prewhitening
gasrx.ar0.out <- esti(gasrx.tsd, model=model.pdq(p=0))
prewhiten(gasrx.tsd, gasry.tsd, x.model=model.pdq(p=0),
          x.coefficients=coef(gasrx.ar0.out))


# -----------------------------------------------------------------------------------
# code for figure on page 11-25
# ?? fixed
gasry.ar4.out <- esti(gasry.tsd, model=model.pdq(p=4))
prewhiten(gasrx.tsd, gasry.tsd, x.model=model.pdq(p=4),
          x.coefficients=coef(gasry.ar4.out))
# -----------------------------------------------------------------------------------
# code for figure on page 11-30 11-32 11-51

gasrx.ar4.out <- esti(gasrx.tsd, model=model.pdq(p=4))
prewhiten(gasrx.tsd, gasry.tsd, x.model=model.pdq(p=4),
          x.coefficients=coef(gasrx.ar4.out))

# -----------------------------------------------------------------------------------
# code for figure on page 11-34 11-35
grate <- ts(c(rep(0,6),gasr.xymat[,1],rep(0,21)))
grate.xmat <- matrix(lag(grate,-3),lag(grate,-4),lag(grate,-5),lag(grate,-6))

esti(gasry.tsd, model=model.pdq(p=3),
     xreg.in=cbind(1,lead.matrix(gasr.xymat[,1],lagvec=c(3,4,5,6))),
     y.range=c(45,65))








