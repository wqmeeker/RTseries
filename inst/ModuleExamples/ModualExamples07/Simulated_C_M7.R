library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 7-5
plot(simnsc.tsd,main = "Simulated Time Series #C")
abline(h=mean(simnsc.tsd),lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 7-6
iden(simnsc.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 7-7
iden(simnsc.tsd,d=1)
# -----------------------------------------------------------------------------------
# code for figure on page 7-8
arima.likelihood.plot(simnsc.tsd ,
                      model = model.pdq(d=1,q = 1), param1 = list(1, seq(-.5, .999, length = 50)))


# -----------------------------------------------------------------------------------
# code for figure on page 7-9
arima.likelihood.plot(simnsc.tsd ,
                      model = model.pdq(d=1,q = 1), param1 = list(1, seq(.5, .95, length = 50)))


# -----------------------------------------------------------------------------------
# code for figure on page 7-10
esti(simnsc.tsd,model=model.pdq(d=1, q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 7-11
esti(simnsc.tsd,model=model.pdq(d=1, q=1))
