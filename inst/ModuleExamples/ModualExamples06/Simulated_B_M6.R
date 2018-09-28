library("RTseries")
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# code for figure on page 6-31
plot(simnsb.tsd,main = "Simulated Time Series #B")
abline(h=mean(simnsb.tsd),lwd=1)
iden(simnsb.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-32
iden(simnsb.tsd, d=1)
