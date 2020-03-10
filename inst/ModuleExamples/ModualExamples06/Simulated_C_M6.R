library("RTseries")
# -----------------------------------------------------------------------------------

simnsc.tsd <- tsd(ts(scan(RTseriesExtDataPath("simnsc.txt")), frequency=1, start=1), data.title="Simulated Time Series C", time.units="Time", response.units="Dollars")



# code for figure on page 6-34
plot(simnsc.tsd, main = "Simulated Time Series #C")
abline(h=mean(simnsc.tsd), lwd=1)
iden(simnsc.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-35
iden(simnsc.tsd, d=1)
