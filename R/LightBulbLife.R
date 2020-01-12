#'
#' @export
LightBulbLife <-
function(number, units, meanHours=1100, sdHours=100){
  ###
  ###   Simulate light bulb life
  ###
  if(missing(number))stop("Must specify the number of observations in the realization.")
  if(missing(units))stop("Must specify the units of time(minutes, hours, or days).")
switch(casefold(units),
       hours={
       mean <- meanHours
     sd<-sdHours
     },
       days={
       mean=meanHours/24
     sd <- sdHours/24},
       minutes={
       mean <- meanHours*60
     sd <- sdHours*60},
       {stop("need to specify units as minutes, hours, or days")})
results <- as.ts(mean + sd*rnorm(number))
results
}
