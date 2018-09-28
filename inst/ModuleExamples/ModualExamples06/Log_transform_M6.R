# code for figure on page 6-21
par(mfrow=c(1,2))
plot(c(1,2,3,4,5,6,7),c(2,4,8,16,32,64,128),type="b",xlab="Time",ylab="Number")
title(main= "100% Growth per Period")
plot(c(1,2,3,4,5,6,7),log(c(2,4,8,16,32,64,128)),
     type="b",xlab="Time",ylab="Log Number")
title(main= "Log of 100% Growth per Period")
# -----------------------------------------------------------------------------------