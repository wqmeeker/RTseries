#------------------------------------------------------------------------------
#Example M6: ATT Stock
#------------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 6-27
att.stock.tsd <- tsd(ts(scan(RTseriesExtDataPath("att_stock.txt")), frequency=1, 	start=1),data.title="1979 Weekly Closing Price of ATT Common Stock",time.units="Week", response.units="Dollars")

iden(att.stock.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 6-27
iden(att.stock.tsd,d=1)

# -----------------------------------------------------------------------------------
