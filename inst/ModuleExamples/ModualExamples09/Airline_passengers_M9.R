#------------------------------------------------------------------------------
#Example M9: International Airline Passengers Descriptive Graphics
#------------------------------------------------------------------------------

library("RTseries")

# -----------------------------------------------------------------------------------
# code for figure on page 9-13

# read the data into a data frame
airline <- read.csv(RTseriesExtDataPath("airlineRegdat.csv"))

Passengers <- airline$Passengers
Passengers.ts <- ts(Passengers,freq=12,start=1949)
Passengers.tsd <- tsd(Passengers.ts, data.title="International Airline Passengers", time.units="Year", response.units="Thousands of Passengers")
iden(Passengers.tsd, gamma=0)

# -----------------------------------------------------------------------------------
# code for figure on page 9-14
iden(Passengers.tsd, gamma=0, d=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-15
iden(Passengers.tsd, gamma=0, D=1)

# -----------------------------------------------------------------------------------
# code for figure on page 9-15
iden(Passengers.tsd, gamma=0, D=1, d=1)
# code for figure on page 9-38 9-39
esti(Passengers.tsd, gamma=0, model=model.pdq(p=1),
     y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-41
esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, D=1, Q=1), y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-42 9-43
esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1), y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-44
esti(Passengers.tsd, gamma=0, model=model.pdq(p=1),
     y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-45 9-46

esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=13, D=1, period=12), fixed=c(NA, rep(0,10), NA, NA))

# complete set of commands for the example
esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, D=1, Q=1, period=12))
#AICc: -467.5581

esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1, period=12))
#AICc: -483.3991

esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=13, D=1, period=12))
#AICc: -477.9514

esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=13, D=1, period=12), fixed=c(NA, rep(0,10), NA, NA))
#AICc: -482.0541


# -----------------------------------------------------------------------------------
# code for figure on page 9-47
esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1), y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-51 9-52
esti(Passengers.tsd, gamma=1, model=model.pdq(d=1, q=1, D=1, Q=1), y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-53 9-56
esti(Passengers.tsd, gamma=0, model=model.pdq(d=1, q=1, D=1, Q=1), y.range=c(100, 1100))

# -----------------------------------------------------------------------------------
# code for figure on page 9-54 9-55
esti(Passengers.tsd, gamma=-0.333, model=model.pdq(d=1, q=1, D=1, Q=1), y.range=c(100, 1100))
