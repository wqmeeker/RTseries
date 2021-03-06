#------------------------------------------------------------------------
#Module 5 Segment 2
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# code for figure on page 5-18
show.true.acfpacf(model=list(ar=0.99999, ma = 0))
# -----------------------------------------------------------------------------------
# code for figure on page 5-19
iden.sim(model=list(ar=c(0.99999)),realization.size=75)

# -----------------------------------------------------------------------------------
# code for figure on page 5-20
iden.sim(model=list(ar=c(0.99999)),realization.size=300)

# -----------------------------------------------------------------------------------
