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

# -----------------------------------------------------------------------------------
# code for figure on page 4-32
arma.roots(c(1.5,.4))
# -----------------------------------------------------------------------------------
# code for figure on page 4-33
arma.roots(c(.5,-.9))
# -----------------------------------------------------------------------------------
# code for figure on page 4-34
arma.roots(c(.5,-.9))
# -----------------------------------------------------------------------------------
# code for figure on page 4-35
arma.roots(c(.5,-.9,.1,.5))

# -----------------------------------------------------------------------------------
# code for figure on page 4-39
show.true.acfpacf(model=list(ma=c(.78,.2)))

# -----------------------------------------------------------------------------------
# code for figure on page 4-40
show.true.acfpacf(model=list(ma=c(1,-.95)))

# -----------------------------------------------------------------------------------
# code for figure on page 4-41
iden.sim(model=list(ma=c(.78,.2)),realization.size=75)
# -----------------------------------------------------------------------------------
# code for figure on page 4-42
iden.sim(model=list(ma=c(.78,.2)),realization.size=300)
# -----------------------------------------------------------------------------------
# code for figure on page 4-43
iden.sim(model=list(ma=c(1,-.95)),realization.size=75)
# -----------------------------------------------------------------------------------
# code for figure on page 4-44
iden.sim(model=list(ma=c(1,-.95)),realization.size=300)

# -----------------------------------------------------------------------------------
