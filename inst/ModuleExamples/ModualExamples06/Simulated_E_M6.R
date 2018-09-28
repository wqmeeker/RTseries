library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 6-42
plot(simnse.tsd,main = "Simulated Time Series #E")
abline(h=mean(simnse.tsd),lwd=1)
iden(simnse.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-43
iden(simnse.tsd, d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 6-44
iden(simnse.tsd, d=2)

