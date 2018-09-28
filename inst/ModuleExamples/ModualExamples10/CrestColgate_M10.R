
#------------------------------------------------------------------------------
#Example M10: CrestColgate
#------------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 10-37
# Colgate Toothpaste Market Share
CrestColgate<-read.csv("http://www.public.iastate.edu/~wqmeeker/anonymous/Stat451_data/CrestColgate.csv")
Colgate.ts <- ts(CrestColgate$Colgate,freq=52,start=1958)
Colgate.tsd <- tsd(Colgate.ts,data.title = "Colgate Market Share Weekly Data",response.units = "Precent")
plot(Colgate.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 10-38
# Crest Toothpaste Market Share
Crest.ts <- ts(CrestColgate$Crest,freq=52,start=1958)
Crest.tsd <- tsd(Crest.ts,data.title = "Crest Market Share Weekly Data",response.units = "Precent")
plot(Crest.tsd)

