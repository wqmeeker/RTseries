
#' Path to RTseries External Data Sets.
#' @description
#' Provides a complete path to external RTseries data sets.
#' @param file The name of the RTseries data set file.
#' @return A character string giving the complete path to the sepecified data set file. If the data set file does not exist, a warning is given and zero-length character string is returned.
#' @examples
#' CarrotPrices <- scan(RTseriesExtDataPath('CarrotPrices.txt'))
#'  # After eading the data, create time series and time series data objects
#' CarrotPrices.ts <- ts(CarrotPrices,frequency=12,start=c(1999,1))
#' CarrotPrices.tsd <- tsd(CarrotPrices.ts, data.title='Carrot Prices from 1999 to 2007',
#'                            response.units='Dollars/cwt', time.units='Year')
#'  
#' BestBuySalesRegdat <- read.csv(RTseriesExtDataPath('BestBuySalesRegdat.csv'))
#'  # Extract the response vector from the data frame
#' BestBuySales <- BestBuySalesRegdat$MillionsOfDollars
#' 
#' ozoneRegdat <- read.csv(RTseriesExtDataPath('ozoneRegdat.csv'))
#' 
#' @export
RTseriesExtDataPath <- function(file = "") {
    the.path <- system.file(package = "RTseries")
    if (the.path == "") {
        warning("RTseries seems not to be loaded\n")
    }
    the.path <- paste(the.path, "/extdata/", file, sep = "")
    
    if (!file.exists(the.path)) {
        warning(paste("Data file ", file, " cannot be found in the RTseries external data folder.\n  Please check the file name.\n"))
        return("")
    }
    the.path
}

