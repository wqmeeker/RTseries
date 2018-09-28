# code for figure on page 9-65
plot(ohio.tsd)
abline(h=mean(ohio.tsd),lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-66
iden(ohio.tsd, gamma=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-67
iden(ohio.tsd, gamma=1,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-68
iden(ohio.tsd, gamma=1,d=2)

# -----------------------------------------------------------------------------------
# code for figure on page 9-69
iden(ohio.tsd, gamma=1,d=3)

# -----------------------------------------------------------------------------------
# code for figure on page 9-70
iden(ohio.tsd, gamma=1,D=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-71
iden(ohio.tsd, gamma=1,D=1,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-74 9-75 9-78
#model 1
esti(ohio.tsd, gamma=1, model=model.pdq(p=2, D=1))

# -----------------------------------------------------------------------------------
# code for figure on page 9-76 9-77 9-81
#model 2
esti(ohio.tsd, gamma=1, model=model.pdq(p=2, D=1, Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 9-79 9-80 9-84
#model 3
esti(ohio.tsd, gamma=1, model=model.pdq(p=2,q=1,D=1,Q=1))

# -----------------------------------------------------------------------------------
# code for figure on page 9-82 9-83 9-89 9-91
#model 5
esti(ohio.tsd, gamma=1, model=model.pdq(d=1, q=1, D=1, Q=1))

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# code for figure on page 9-93

arima.contour.plot(ohio.tsd,
                   model = model.pdq(d=1, q=1, D=1, Q=1),
                   list(1, seq(-0.99, 0.99, length = 40)),
                   list(2, seq(-0.99, 0.99, length = 40)))

# -----------------------------------------------------------------------------------
# code for figure on page 9-94

arima.contour.plot(ohio.tsd, relative = T,
                   model = model.pdq(d=1, q=1, D=1, Q=1),
                   list(1, seq(0.25, 0.65, length = 40)),
                   list(2, seq(0.39, 0.79, length = 40)))
# -----------------------------------------------------------------------------------
# code for figure on page 9-95

arima.contour.plot(ohio.tsd, persp.plot = T,relative = T,
                   model = model.pdq(d=1, q=1, D=1, Q=1),
                   list(1, seq(0.25, 0.65, length = 40)),
                   list(2, seq(0.39, 0.79, length = 40)))
# -----------------------------------------------------------------------------------
# code for figure on page 9-88 9-90 9-96
#model 6
esti(ohio.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1))
#------------------------------------------------------------------------------------
ohio.model6.out<-esti(ohio.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1))
show.acf(ohio.model6.out$resid)

