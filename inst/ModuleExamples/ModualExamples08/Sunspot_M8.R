#------------------------------------------------------------------------
#Example M8: sunspot
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 8-8
plot(spot.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 8-9
iden(data.tsd = spot.tsd)

# ----------------------------------------------------------------------------------
# code for figure on page 8-10
iden(spot.tsd,gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-11 8-12
esti(data.tsd = spot.tsd, model =model.pdq(p = 1),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-14 8-15
esti(data.tsd = spot.tsd, model =model.pdq(p = 2),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-16
esti(data.tsd = spot.tsd, model =model.pdq(p = 1),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-17 8-18
esti(data.tsd = spot.tsd, model =model.pdq(p = 2))

# ----------------------------------------------------------------------------------
# code for figure on page 8-19
esti(data.tsd = spot.tsd, model =model.pdq(p = 2),gamma=0.50)
# ----------------------------------------------------------------------------------

# code for figure on page 8-20
# ?? fixed
# Wolfer Sunspot Data
arima.contour.plot(spot.tsd,
                   model = model.pdq(p = 2),
                   list(1, seq(-1.99, 1.99, length = 40)),
                   list(2, seq(-0.99, 0.99, length = 40)))
# ----------------------------------------------------------------------------------
# code for figure on page 8-21
# ?? fixed
# Wolfer Sunspot Data
arima.contour.plot(spot.tsd,
                   model = model.pdq(p = 2),
                   list(1, seq(.79, 1.99, length = 40)),
                   list(2, seq(-0.99, 0.19, length = 40)))
# ----------------------------------------------------------------------------------
# code for figure on page 8-23 8-24
esti(data.tsd = spot.tsd, model =model.pdq(p = 3),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-25
esti(data.tsd = spot.tsd, model =model.pdq(p = 2),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-26 8-27
esti(data.tsd = spot.tsd, model =model.pdq(p = 16),gamma=0.50)

# ----------------------------------------------------------------------------------
# code for figure on page 8-28
esti(data.tsd = spot.tsd, model =model.pdq(p = 2),gamma=0.50)

