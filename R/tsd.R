#' Create a Time Series Data Object
#' @description
#' Creates a time-series data object that contains a time series and related information, e.g. data structure, title and the units. Provide input for esti and iden.
#' @param data.ts A time series object (created with the R \code{\link{ts}} function). This data should contains start date, end date and frequency.
#' @param data.title Optional. Title for the data. Default is the name of the data.ts object.
#' @param time.units Optional. Time units of the data. Default is 'Time'.
#' @param response.units Optional. The units of time for the time series data. Default is the name of the data.ts object.
#' @return This function returns a time series data object that can be used as input to the \code{\link{iden}}, \code{\link{esti}}, and other RTsereis functions.
#' @seealso \code{\link{iden}}, \code{\link{esti}}
#' @examples
#' savings.rate.ts <- ts(savings.rate, frequency=4, start=1955)
#' savings.rate.tsd <- tsd(savings.rate.ts, data.title='Change in Inventory',
#'      response.units='Billions of Dollars', time.units='Year')
#' iden(savings.rate.tsd)
#' @export
tsd <- function(data.ts, data.title = deparse(substitute(data.ts)), time.units = "Time", response.units = deparse(substitute(data.ts))) {
    the.class <- class(data.ts)
    if (the.class != "ts") 
        stop(paste("The class of input data is ", the.class, ". It should be a ts (time series) object.\n", 
            sep = ""))
    attr(data.ts, "time.units") <- time.units
    attr(data.ts, "data.title") <- data.title
    attr(data.ts, "response.units") <- response.units
    class(data.ts) <- c("tsd", "ts")
    data.ts
}
