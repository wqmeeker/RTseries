library("RTseries")
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# code for figure on page 6-47
plot(simnsa.tsd,main = "Simulated Time Series #A")
abline(h=mean(simnsa.tsd),lwd=1)
iden(simnsa.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-48
iden(simnsa.tsd, d=1)
