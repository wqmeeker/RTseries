
#'
#' Seasonal Prediction
#' @description
#' Provides a plot of predictions of a linear regression model using dummy variables for seasonal effects.
#' @param the.fit  Output from the lm command fitting a dummy variable seasonal time series model.
#' @param the.data.frame Data frame used in itting the dummy variable seasonal time series model.
#'
#' @importFrom stats predict
#' @export
seasonal.predictions <- function(the.fit, the.data.frame) {
    # function to compute predictions from a dummy-variable regression model
    names.the.data.frame <- names(the.data.frame)
    Time <- the.data.frame$Time
    the.length <- length(Time)
    # Check if quarterly
    if (is.element("Quarter", names.the.data.frame)) {
        Quarter <- the.data.frame$Quarter
        # look for log in call
        if (regexpr("log", as.character(the.fit$call)[2]) > 0) {
            # loop over the quarters
            for (the.quarter in sort(unique(as.character(Quarter)))) {
                lines(Time, exp(predict(the.fit, newdata = data.frame(Time = Time, Quarter = rep(the.quarter, 
                  length = the.length)))))
            }
        } else {
            # loop over the quarters
            for (the.quarter in sort(unique(as.character(Quarter)))) {
                lines(Time, predict(the.fit, newdata = data.frame(Time = Time, Quarter = rep(the.quarter, 
                  length = the.length))))
            }
        }
    } else {
        # Check if monthly
        Month <- the.data.frame$Month
        if (is.null(Month)) 
            stop("The input the.data.frame must have either a Quarter or a Month variable\n")
        # look for log in call
        if (regexpr("log", as.character(the.fit$call)[2]) > 0) 
            for (the.month in sort(unique((as.character(Month))))) {
                lines(Time, exp(predict(the.fit, newdata = data.frame(Time = Time, Month = rep(the.month, 
                  length = the.length)))))
            } else for (the.month in sort(unique((as.character(Month))))) {
            print(the.month)
            lines(Time, (predict(the.fit, newdata = data.frame(Time = Time, Month = rep(the.month, length = the.length)))))
        }
    }
}
