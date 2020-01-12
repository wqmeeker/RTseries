#'
#' @export
Profit <-
function(number, units, meanRevenueDollars=200, sdRevenueDollars=20, meanExpensesDollars=180, sdExpensesDollars=10){
  ###
  ###   Simulate profit/loss
  ###
  if(missing(number))stop("Must specify the number of observations in the realization.")
  if(missing(units))stop("Must specify the units of currency (euros, dollars, or cny ).")
switch(casefold(units),
       dollars={
       meanRevenue <- meanRevenueDollars
     sdRevenue <- sdRevenueDollars
     meanExpenses <- meanExpensesDollars
     sdExpenses <-sdExpensesDollars
     },
       euros={
       meanRevenue <- 1.31646*meanRevenueDollars
     sdRevenue <- 1.31646*sdDevenueDollars
       meanExpenses=1.31646*meanExpensesDollars
     sdExpenses <- 1.31646*sdmeanExpensesollars},
       cny ={
       meanRevenue <- meanRevenueDollars*0.1476
     sdRevenue <- sdRevenueDollars*0.1476
       meanExpenses <- meanExpensesDollars*0.1476
     sdExpenses <- sdExpensesDollars*0.1476},
       {stop("need to specify units as euros, dollars, or cny")})
  the.ts.errorsExpenses <- arima.sim(n=number, list(ar = 0.001))
  the.ts.errorsRevenue <- arima.sim(n=number, list(ar = 0.001))
  resultsExpenses <- meanExpenses + sdExpenses*the.ts.errorsExpenses
  resultsRevenue <- meanRevenue + sdRevenue*the.ts.errorsRevenue
resultsProfit <- resultsRevenue - resultsExpenses
results <- cbind(Revenue=resultsRevenue, Expenses=resultsExpenses, Profit=resultsProfit)
results
}
