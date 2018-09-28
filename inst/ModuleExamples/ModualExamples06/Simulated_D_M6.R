library("RTseries")
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# code for figure on page 6-38
plot(simnsd.tsd,main = "Simulated Time Series #D")
abline(h=mean(simnsd.tsd),lwd=1)
iden(simnsd.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-39
iden(simnsd.tsd, d=1)

