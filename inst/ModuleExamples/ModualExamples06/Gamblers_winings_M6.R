# code for figure on page 6-25
par(mfrow=c(2,1))
plot(c(1,2,3,4,5,6,7,8,9,10,11),c(100,110,108,115,119,118,125,127,125,134,140),
     type="b",xlab="Time",ylab="Dollars")
title(main= "Cumulative Winnings")
plot(c(2,3,4,5,6,7,8,9,10,11),c(10,-2,7,4,-1,7,2,-2,9,6),
     type="b",xlab="Time",ylab="Dollars")
title(main= "Incremental Winnings")
# -----------------------------------------------------------------------------------
