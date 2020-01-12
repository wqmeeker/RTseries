#'
#' @export
plot.ts.deviations <-
function(ts, cex.axis = par("cex.axis"), xaxs = "r", yaxs = "r"){
mean.ts <- mean(ts)

stats:::plot.ts
plot(ts)
abline(h=mean.ts, lwd=3, col=4)
the.index <- 1:length(ts)

 plot(x = range(the.index), y = range(ts), type = "n", xaxs = xaxs, 
        yaxs = yaxs, xlab = "", ylab = "", cex.axis = cex.axis)

abline(h=mean.ts, lwd=3, col=4)
for(i in 1:length(ts)){
plot.line(point1=c(i,mean.ts), point2=c(i,ts[i]))
}
}
