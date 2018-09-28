library("RTseries")
# -----------------------------------------------------------------------------------

# code for figure on page 6-34
plot(simnsc.tsd,main = "Simulated Time Series #C")
abline(h=mean(simnsc.tsd),lwd=1)
iden(simnsc.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-35
iden(simnsc.tsd, d=1)
