#------------------------------------------------------------------------
#Example M7: device inventory
#------------------------------------------------------------------------

library("RTseries")
device.inventory.tsd <-  tsd(ts(scan(RTseriesExtDataPath("device_inventory.txt")), frequency=12,start=1984),data.title="Device Inventory 1984-1994", response.units="Hundreds of Devices",time.units="Year")

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# code for figure on page 7-13
plot(device.inventory.tsd,main = "Device Inventory 1984-1994")
abline(h=0,lwd=1)

# -----------------------------------------------------------------------------------
# code for figure on page 7-14
iden(device.inventory.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 7-15
iden(device.inventory.tsd,d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 7-16
iden(device.inventory.tsd,d=2)

# -----------------------------------------------------------------------------------
# code for figure on page 7-18
arima.likelihood.plot(device.inventory.tsd,
                      model = model.pdq(p = 1), param1 = list(1, seq(.8, .999, length = 50)))


# -----------------------------------------------------------------------------------
# code for figure on page 7-19
#??
arima.likelihood.plot(device.inventory.tsd,
                      model = model.pdq(p = 1), list(1, seq(.97, .999, length =50)))

# -----------------------------------------------------------------------------------
# code for figure on page 7-21,7-22
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 1), y.range =c(-12,22) )

# -----------------------------------------------------------------------------------
# code for figure on page 7-24,7-25
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 2) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-35
arima.contour.plot(device.inventory.tsd,
              model = model.pdq(p = 2),
              list(1, seq(-1.9, 1.9, length = 50)),
              list(2, seq(-.99, .99, length = 50)))

arima.contour.plot(device.inventory.tsd,
              model = model.pdq(p = 2),
              list(1, seq(1.377, 1.394, length = 40)),
              list(2, seq(-0.406, -0.390, length = 40)))

# -----------------------------------------------------------------------------------
# code for figure on page 7-36
# Device Inventory Data
arima.contour.plot(device.inventory.tsd,
                   model = model.pdq(p = 2),
                   list(1, seq(1.200, 1.794, length = 40)),
                   list(2, seq(-0.6, -0.390, length = 40)))
# -----------------------------------------------------------------------------------
# code for figure on page 7-37
# Device Inventory Data
arima.contour.plot(device.inventory.tsd,
                   model = model.pdq(p = 2),
                   list(1, seq(1.375, 1.395, length = 40)),
                   list(2, seq(-0.405, -0.390, length = 40)))
# code for figure on page 7-39, 7-40
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 3) ,y.range =c(-12,22))
# -----------------------------------------------------------------------------------
# code for figure on page 7-41
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 2) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-43 7-44
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 1,d=1,q=0) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-45
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 3) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-46 7-47
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 1,d = 1,q = 1) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-48
esti(data.tsd = device.inventory.tsd, model =model.pdq(p = 1,d=1,q=0) ,y.range =c(-12,22))

# -----------------------------------------------------------------------------------
# code for figure on page 7-49
# Device Inventory Data
arima.contour.plot(device.inventory.tsd,
                   model = model.pdq(p = 1, d = 1, q = 1),
                   list(1, seq(-0.99, 0.99, length = 40)),
                   list(2, seq(-0.99, 0.99, length = 40)))

# -----------------------------------------------------------------------------------
# code for figure on page 7-50
# ??fixed
# Device Inventory Data
arima.contour.plot(device.inventory.tsd, persp.plot = T,
                   model = model.pdq(p = 1, d = 1, q = 1),
                   list(1, seq(-0.99, 0.99, length = 40)),
                   list(2, seq(-0.99, 0.99, length = 40)))

# -----------------------------------------------------------------------------------
# code for figure on page 7-51
# Device Inventory Data
arima.contour.plot(device.inventory.tsd,
                   model = model.pdq(p = 1, d = 1, q = 1),
                   list(1, seq(-0.4, 0.89, length = 40)),
                   list(2, seq(-0.49, 0.59, length = 40)))

# -----------------------------------------------------------------------------------
# code for figure on page 7-52
# ??fixed
# Device Inventory Data
arima.contour.plot(device.inventory.tsd, persp.plot = T,
                   model = model.pdq(p = 1, d = 1, q = 1),
                   list(1, seq(-0.4, 0.89, length = 40)),
                   list(2, seq(-0.49, 0.59, length = 40)))






