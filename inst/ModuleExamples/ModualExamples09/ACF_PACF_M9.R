library("RTseries")

# -----------------------------------------------------------------------------------
# code for figure on page 9-20
show.true.acfpacf(model=list(ma=c(rep(0, 11), +0.9)),
                  nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-21
show.true.acfpacf(model=list(ma=c(rep(0, 11), -0.9)),
                  nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-23
show.true.acfpacf(model=list(ma=0, ar=c(rep(0, 11), +0.9)),
                  nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-24
show.true.acfpacf(model=list(ma=0, ar=c(rep(0, 11), -0.9)),
                  nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-28
show.true.acfpacf(model=list(ma=c(0.5,rep(0,10),0.9,-0.45)),nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-29
show.true.acfpacf(model=list(ma=c(0.1,rep(0,10),0.9,-0.09)),nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-30
show.true.acfpacf(model=list(ma=c(-0.5,rep(0,10),-0.9,-0.45)),nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-31
show.true.acfpacf(model=list(ma=0,ar=c(+0.5,rep(0,10),+0.9,-0.45)),nacf=38)

# -----------------------------------------------------------------------------------
# code for figure on page 9-32
show.true.acfpacf(model=list(ma=0,ar=c(+0.1,rep(0,10),+0.9,-0.09)),nacf=38)
# -----------------------------------------------------------------------------------
# code for figure on page 9-33
show.true.acfpacf(model=list(ma=0,ar=c(-0.5,rep(0,10),-0.9,-0.45)),nacf=38)
# -----------------------------------------------------------------------------------
# code for figure on page 9-34
show.true.acfpacf(model=list(ar=c(rep(0,11), -0.9), ma=c(rep(0, 11), +0.9)))

# -----------------------------------------------------------------------------------
# code for figure on page 9-35
show.true.acfpacf(model=list(ar=c(rep(0,11), +0.9), ma=c(rep(0, 11), -0.9)))

