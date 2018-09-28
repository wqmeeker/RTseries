library("RTseries")
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# code for figure on page 6-50
plot(simnsf.tsd,main = "Simulated Time Series #F")
abline(h=mean(simnsf.tsd),lwd=1)
iden(simnsf.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-51
iden(simnsf.tsd, d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 6-52
iden(simnsf.tsd, d=2)








