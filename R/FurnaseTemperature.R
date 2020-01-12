#'
#' @export
FurnaseTemperature <-
function(number, units, meanC=200, sdC=3, phi=0.7){
  ###
  ###   Simulate furnase temperature
  ###
  if(missing(number))stop("Must specify the number of observations in the realization.")
  if(missing(units))stop("Must specify the units of temperature (F, C, or K).")
switch(casefold(units),
       f={
       mean <- (9/5)*meanC +32
       sd<-sdC*(9/5)
     },
       c={
       mean=meanC
       sd <- sdC},
       k={
       mean <- meanC + 273.15
     sd <- sdC},
       {stop("need to specify units as temperature F, C, or K")})
  the.ts.errors <- arima.sim(n=number, list(ar = phi))
  results <- mean + sd*the.ts.errors*sqrt(1-phi^2)
results
}
