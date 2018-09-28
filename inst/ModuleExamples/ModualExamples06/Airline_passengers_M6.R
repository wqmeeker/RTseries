#------------------------------------------------------------------------------
#Example M6: International Airline Passengers 
#------------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 6-5
airline <- read.csv(RTseriesExtDataPath("airlineRegdat.csv"))
Passengers <- airline$Passengers
Passengers.ts <- ts(Passengers,freq=12,start=1949)
Passengers.tsd <- tsd(Passengers.ts,
                      data.title='International Airline Passengers',
                      response.units='Thousands of Passengers', time.units='Year')
plot(Passengers.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-6
iden(Passengers.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-7 6-13
iden(Passengers.tsd, gamma = 0)

# -----------------------------------------------------------------------------------
# code for figure on page 6-12
iden(Passengers.tsd, gamma = 0.5)

# -----------------------------------------------------------------------------------
# code for figure on page 6-14
iden(Passengers.tsd, gamma = -0.3333)

# -----------------------------------------------------------------------------------
# code for figure on page 6-15
iden(Passengers.tsd, gamma = -1)

# -----------------------------------------------------------------------------------
