library("RTseries")
# -----------------------------------------------------------------------------------

# code for figure on page 4-11
plot(arima.sim(n=250, model=list(ma=0.90)),
     ylab="")

# -----------------------------------------------------------------------------------
# code for figure on page 4-12
plot(arima.sim(n=250, model=list(ma=-0.90)),
     ylab="")
# -----------------------------------------------------------------------------------
# code for figure on page 4-17
show.true.acfpacf(model=list(ma=.95))

# -----------------------------------------------------------------------------------
# code for figure on page 4-18
show.true.acfpacf(model=list(ma=-.95))

# -----------------------------------------------------------------------------------
# code for figure on page 4-19
iden.sim(model=list(ar=c(0),ma=c(.95)),realization.size=75)

# -----------------------------------------------------------------------------------
# code for figure on page 4-20
iden.sim(model=list(ar=c(0),ma=c(.95)),realization.size=300)

# -----------------------------------------------------------------------------------
# code for figure on page 4-21
iden.sim(model=list(ar=c(0),ma=c(-.95)),realization.size=75)
# -----------------------------------------------------------------------------------
# code for figure on page 4-22
iden.sim(model=list(ar=c(0),ma=c(-.95)),realization.size=300)
