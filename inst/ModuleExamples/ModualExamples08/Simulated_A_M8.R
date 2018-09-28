library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 8-48
iden(simnsa.tsd)

# ----------------------------------------------------------------------------------
# code for figure on page 8-49
iden(simnsa.tsd, d =1)

# ----------------------------------------------------------------------------------
# code for figure on page 8-50 8-51
esti(data.tsd = simnsa.tsd, model =model.pdq(p = 0,d = 1,q = 2 ))

# ----------------------------------------------------------------------------------
# code for figure on page 8-52 8-53
esti(data.tsd = simnsa.tsd, model =model.pdq(p = 0,d = 1,q = 2 ),d.trend = T)

# ----------------------------------------------------------------------------------
# code for figure on page 8-54
esti(data.tsd = simnsa.tsd, model =model.pdq(p = 0,d = 1,q = 2 ))

# ----------------------------------------------------------------------------------
# code for figure on page 8-56
arima.contour.plot(simnsa.tsd,
                   model = model.pdq(p = 0,d = 1,q = 2 ),
                   list(1, seq(-1.99, 1.99, length = 40)),
                   list(2, seq(-0.99, 0.99, length = 40)))
abline(v=1.4)
abline(v=1.6)
abline(h=-0.6)
abline(h=-0.4)
# ----------------------------------------------------------------------------------
# code for figure on page 8-57
arima.contour.plot(simnsa.tsd,
                   model = model.pdq(p = 0,d = 1,q = 2 ),
                   list(1, seq(1.4, 1.65, length = 40)),
                   list(2, seq(-0.8, -0.2, length = 40)))
# ----------------------------------------------------------------------------------
# code for figure on page 8-58

arima.contour.plot(simnsa.tsd, persp.plot=T,
                   model = model.pdq(p = 0,d = 1,q = 2 ),
                   list(1, seq(1.4, 1.65, length = 40)),
                   list(2, seq(-0.8, -0.2, length = 40)))


