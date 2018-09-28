
#------------------------------------------------------------------------
#Example M3: change in inventory data
#    Appeared as Case 1 in Pankrantz (1983)
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 3-7
change.inventory.ts <- ts(scan(RTseriesExtDataPath("ChangeInventory.txt")), start=1955, freq=4)

change.inventory.tsd <- tsd(change.inventory.ts,time.units="Year",data.title="Change in Inventories",response.units="Billions of Dollars")
plot(change.inventory.tsd)

# -----------------------------------------------------------------------------------
# code for figure on page 3-9
plot(change.inventory.tsd)

abline(h = mean(change.inventory.tsd), lwd = 2, col = 6)
abline(h=mean(change.inventory.tsd)-2*sqrt(var(change.inventory.tsd)),col=6,lty=6,lwd=2)
abline(h=mean(change.inventory.tsd)+2*sqrt(var(change.inventory.tsd)),col=6,lty=6,lwd=2)
