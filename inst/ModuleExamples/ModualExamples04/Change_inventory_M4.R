#------------------------------------------------------------------------
#Example M4:  change in inventory data
#    Appeared as Case 1 in Pankrantz (1983)
#------------------------------------------------------------------------

library("RTseries")
# -----------------------------------------------------------------------------------
# code for figure on page 4-4

change.inventory.ts <- ts(scan(RTseriesExtDataPath("ChangeInventory.txt")), start=1955, freq=4)

change.inventory.tsd <- tsd(change.inventory.ts,time.units="Year",data.title="Change in Inventories",response.units="Billions of Dollars")

par(mfrow=c(2,1))
plot(change.inventory.ts,xlab="Year",ylab="Billions of Dollars")
title("Change in Inventories")
abline(h=mean(change.inventory.ts))
plot(change.inventory.ts-mean(change.inventory.ts),xlab="Year",ylab="Billions of Dollars")
title("Centered Change in Inventories")
abline(h=0)
par(mfrow=c(1,1))
