#------------------------------------------------------------------------------
#Example 1: Housing Start Data (built into RTSERIES)
#------------------------------------------------------------------------------

library("RTseries")
# code for figure on page 1-7
hstart
hstart.ts <- ts(hstart,frequency = 12, start = c(1966, 1))
hstart.ts
plot(hstart)
plot(hstart.ts)
plot(hstart.ts,xlab="Year",ylab="Thousands of Homes")
title(main="US Housing Starts 1966-1974")

# code for figure on page 1-29
plot(hstart.ts,xlab="Year",ylab="Thousands of Homes")
title(main="US Housing Starts 1966-1974")
# -----------------------------------------------------------------------------------
# code for figure on page 1-30
shaded.tsplot(hstart.ts)
title(main="US Housing Starts 1966-1974",xlab="Year",ylab="Thousands of Homes")
legend(1971,75,"US Housing Starts")
# -----------------------------------------------------------------------------------
# code for figure on page 1-31
shaded.tsplot(hstart.ts,top=T)
title(main="US Housing Starts 1966-1974",xlab="Year",ylab="Thousands of Homes")
legend(1971,75,"US Housing Starts")
# -----------------------------------------------------------------------------------

