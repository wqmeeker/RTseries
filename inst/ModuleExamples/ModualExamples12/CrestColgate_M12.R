#------------------------------------------------------------------------------
#Example M12: CrestColgate
#------------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 12-4
plot(Colgate.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 12-5
plot(Crest.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 12-6
iden(Crest.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 12-7
iden(Crest.tsd,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 12-8 12-9 12-14 12-17
# Model 1 IMA(1,1)
esti(Crest.tsd, model=model.pdq(d=1,q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 12-12 12-13 12-20
# Model 2 ARMA(1,1)
esti(Crest.tsd, model=model.pdq(p=1,q=1))



#create the step function for the intervention models
crestStep <- matrix(c(rep(0,134), 1, rep(1,141+24)), ncol=1)

# -----------------------------------------------------------------------------------
# code for figure on page 12-15 12-16 12-21
# Model 3 IMA(1,1) with Intervention Term
esti(crest.tsd, model=model.pdq(d=1, q=1),xreg=crestStep, y.range=c(0, 0.6))

# -----------------------------------------------------------------------------------
# code for figure on page 12-18 12-19
# Model 4 ARMA(1,1)with Intervention Term
esti(crest.tsd, model=model.pdq(p=1, q=1),xreg=crestStep, y.range=c(0, 0.6))








