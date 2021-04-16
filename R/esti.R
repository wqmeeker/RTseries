#' ARIMA Model Estimation
#' @description
#' Performas estimation, diagnostic checking, and forecasting for a specified ARIMA model and time series data object.
#' @param data.tsd Input time series data object (output from the tsd function).
#' @param gamma Optional; the Box-Cox transformation power parameter. The default value is gamma=1 (no transformation). If a log transformation is needed, use gamma=0.
#' @param m Optional constant to added to the response variable before the data are transformed. The default value is m=0.
#' @param model A list specifying an ARIMA model. See functions model.pdq for a simple method for specifying the model input.
#' @param gof.lag Number of lags to be use to compute the Ljung-Box statistic (default=10).
#' @param lag.max The number of lags at which to estimate residual autocorrelations (default is lag.max=38).
#' @param number.forecasts How far into the future to forecast (default is number.forecast=24).
#' @param pred.level Prediction confidence level (default is pred.level =0 .95).
#' @param xreg.in An x matrix for dynamic regression (see \code{\link{lead.matrix}}).
#' @param y.range Range of the y axis for the predictions
#' @param x.range Range of the x axis for the predictions
#' @param d.trend  Indicates whether a model should have a deterministic trend term or not (default is  d.trend = FALSE).
#' @param print.table If print.table = TRUE, a table of the forecasts and prediction intervals will be provided (default is print.table = FALSE). 
#' @param ps1 if provided, the postscript file name for the first page of graphical output
#' @param ps2 if provided, the postscript file name for the second page of graphical output
#' @param ... allows sending down extra arguments to the arima function (such as optimization options or fixed values of parameters).
#' @return Invisibly returns the output from the R \code{\link{arima}} command used to do the estimation.
#' @examples
#' # AR(1) model for the sunspot data
#' esti(spot.tsd, model=model.pdq(p=3))
#' 
#' # Airline model for the international airline passengers data
#' Passengers.ts <- ts(Passengers, freq=12, start=1949)
#' Passengers.tsd <- tsd(Passengers.ts, data.title='International Airline Passengers',
#'                       time.units='Year', response.units='Thousands of Passengers')
#' airline.model <- model.pdq(period=12, d=1, D=1, q=1, Q=1)
#' esti(Passengers.tsd, gamma = 0, model=airline.model)
#' esti(Passengers.tsd, gamma = 0, m = 2, model=airline.model)
#' 
#' @importFrom graphics abline
#' @importFrom stats acf
#' @importFrom stats arima
#' @importFrom graphics close.screen
#' @importFrom grDevices dev.off
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom grDevices postscript
#' @importFrom graphics screen
#' @importFrom graphics split.screen
#' @importFrom stats predict
#' @importFrom stats qqnorm
#' @importFrom stats qnorm
#' @importFrom stats residuals
#' @importFrom graphics title
#' @importFrom stats ts
#' @importFrom stats tsp
#' @export
"esti" <- function(data.tsd, gamma = 1, m = 0, model, gof.lag = 10, lag.max = 38, number.forecasts = 24, 
    pred.level = 0.95, xreg.in, y.range, x.range, d.trend = FALSE, print.table = FALSE, ps1 = NULL, ps2 = NULL, 
    ...) {
    # 19 march 2016 modified to work with postscript (after Splus)
    
    missing.xreg.in <- missing(xreg.in)
    original.par <- TRUE
    series.name <- deparse(substitute(data.tsd))
    if (!missing.xreg.in) 
        xname <- deparse(substitute(xreg.in)) else xname <- ""
    
    time.units <- attr(data.tsd, "time.units")
    data.title <- attr(data.tsd, "data.title")
    response.units <- attr(data.tsd, "response.units")
    ylab <- response.units
    
    # pick up the seasonal period from data if not specified
    model.in <- model
    model <- complete.model(model, data.tsd)
    
    # processing for postscript graphics
    if (!missing(ps1)) 
        postscript(file = ps1)
    old.par <- par()
    old.options <- options()
    on.exit({
        options(old.options)
        
        old.par[c("cin", "cra", "csi", "cxy", "din", "page")] <- NULL
        
        
        par(old.par)
        par(new = FALSE)
    })
    model.string <- f.model.string(model)
    {
        {
            cat(paste("Estimation/Forecasting Output for", data.title, "\n", model.string, "\n"))
            fig1 <- matrix(c(0, 1, 0.5, 1, 0, 0.55, 0, 0.45, 0.55, 1, 0, 0.45), ncol = 4, byrow = TRUE)
            if (m == 0) {
                char.m <- NULL
            } else {
                char.m <- paste("+", format(m))
            }
            ylab <- response.units
            if (gamma == 0) {
                y.trans <- log(data.tsd + m)
                trans.ylab <- paste("log(", ylab, char.m, ")")
            } else {
                if (gamma == 1) {
                  y.trans <- data.tsd + m
                  trans.ylab <- paste(ylab, char.m)
                } else {
                  if (gamma < 0) {
                    y.trans <- wqm.Uminus(((data.tsd + m)^gamma))
                    trans.ylab <- paste("-(", ylab, char.m, ")", "^", format(gamma))
                  } else {
                    y.trans <- ((data.tsd + m)^gamma)
                    trans.ylab <- paste("(", ylab, char.m, ")", "^", format(gamma))
                  }
                }
            }
            # defaults for no regression or constant term
            newxreg <- NULL
            xregstr <- ""
            
            # allow for deterministic trend by adding column of ones
            
            if (missing.xreg.in) {
                
                
                
                # allow for deterministic trend by adding column of ones
                if (is.diff(model) && d.trend) {
                  xregstr <- "trend term"
                }
                
                
                
                
                xreg <- NULL
            } else {
                # strip off any forcast-length values to do the estimation
                if (nrow(as.matrix(xreg.in)) != length(y.trans) + number.forecasts) {
                  number.forecasts.update <- nrow(as.matrix(xreg.in)) - length(y.trans)
                  warning(paste("The number of rows in the xmatrix", nrow(as.matrix(xreg.in)), "should be equal to the length of the time series", 
                    length(y.trans), "\nplus the number of forecasts (default is", number.forecasts, "); adjusting to", 
                    paste(number.forecasts.update, ".", sep = ""), "\n"))
                  number.forecasts <- number.forecasts.update
                }
                # separate out the regression x matrix and the newdata
                xreg <- xreg.in[1:length(y.trans), ]
                newxreg <- xreg.in[(length(y.trans) + 1):nrow(xreg.in), , drop = F]
                if (!missing.xreg.in) 
                  xregstr <- paste("regr variables=", xname)
            }
            arima.out <- arima(x = y.trans, order = model$local, seasonal = model$seasonal, xreg = xreg, 
                ...)
            if (is.null(list(...)$fixed)) {
                my.fixed <- rep(NA, length(arima.out$coef))
            } else my.fixed <- list(...)$fixed
            arima.out$series.name <- series.name
            arima.out$wqm.model <- model
            arima.out$fixed <- my.fixed
            print.arima.out <- print.arima(arima.out)
            number.parameter <- print.arima.out$number.parameter
            if (!is.null(newxreg)) {
                if (number.forecasts != nrow(newxreg)) {
                  warning("Number of forecasts requested does not equal the number of extra X rows")
                  number.forecasts <- nrow(newxreg)
                }
                predict.Arima.out <- predict(arima.out, n.ahead = number.forecasts, newxreg = newxreg)
            } else {
                predict.Arima.out <- predict(arima.out, n.ahead = number.forecasts)
            }
            # allow for deterministic trend by adding column of ones
            the.differenced.series <- difference.series(y.trans, model)
            the.mean <- mean(the.differenced.series)
            if (!is.diff(model)) 
                cat("The mean of the time series is", the.mean, "\n") else cat("The mean of the differenced time series is", the.mean, "\n")
            if (is.diff(model) && d.trend) {
                the.trend <- get.trend.prediction(predict.Arima.out, model, the.mean)
            } else the.trend <- rep(0, length(predict.Arima.out$pred))
            the.residuals <- residuals(arima.out)
            fitted.values <- y.trans - the.residuals
            close.screen(all.screens = TRUE)
            split.screen(fig1)
            screen(1)
            par(mai = c(0.4, 0.5, 0.7, 0.1))
            plot(the.residuals, ylab = "Residuals", xlab = time.units, type = "n")
            mtext("Residuals versus Time", side = 3, line = 1, cex = 1, font = 2, outer = FALSE)
            lines(the.residuals, lwd = 2, col = 2)
            abline(h = 0)
            trans.string <- paste("w=", trans.ylab)
            mtext(text = data.title, side = 3, line = 2, cex = 1, outer = FALSE)
            model.trans.string <- paste(model.string, "on", trans.string, xregstr)
            cat(paste(model.trans.string, "\n"))
            mtext(text = model.trans.string, side = 3, line = 0.2, outer = FALSE)
            screen(3)
            acf.out <- acf(strip.na(the.residuals), lag.max = lag.max, type = "correlation", plot = FALSE)
            # acf.out <- acf(the.residuals, lag.max = lag.max, type = 'correlation', plot = FALSE)
            acf.out$type <- "rcorrelation"
            par(mai = c(0.6, 0.5, 0.3, 0.1))
            my.acf.plot(acf.out, data.tsd, number.parameter = number.parameter, print.table = print.table, seasonal.lags=TRUE)
            screen(2)
            par(mai = c(0.4, 0.5, 0.7, 0.1))
            plot(as.vector(fitted.values), as.vector(the.residuals), xlab = "Fitted Values", ylab = "Residuals", 
                type = "n")
            title(main = "Residuals versus Fitted Values")
            
            points(as.vector((y.trans - the.residuals)), as.vector(the.residuals), lwd = 2, col = 2)
            abline(h = 0)
        }
    }
    if (!is.null(ps1)) {
        dev.off()
        if (original.par) 
            par(old.par)
    }
    # now beginning the second diagnostic plot
    if (!is.null(ps2)) {
        postscript(file = ps2)
        old.par <- par()
    } else {
        pause()
    }
    {
        fig2 <- matrix(c(0, 1, 0.45, 1, 0.275, 0.725, 0, 0.45), ncol = 4, byrow = TRUE)
        actual <- y.trans
        # had to make future into a time sereis, because in ver 3.4 splus stopped putting these in
        # automatically
        future <- ts(predict.Arima.out$pred + the.trend, start = tsp(actual)[2] + 1/tsp(actual)[3], frequency = tsp(actual)[3])
        se.future <- predict.Arima.out$se
        z.value <- qnorm(1 - (1 - pred.level)/2)
        
        upper.future <- future + z.value * se.future
        lower.future <- future - z.value * se.future
        # ts attributes seem not to pass correctly
        upper.future <- ts(upper.future, start = tsp(actual)[2] + 1/tsp(actual)[3], frequency = tsp(actual)[3])
        lower.future <- ts(lower.future, start = tsp(actual)[2] + 1/tsp(actual)[3], frequency = tsp(actual)[3])
        # do the inverse box-cox
        
        if (gamma == 0) {
            actual <- exp(actual) - m
            fitted.values <- exp(fitted.values) - m
            future <- exp(future) - m
            upper.future <- exp(upper.future) - m
            lower.future <- exp(lower.future) - m
        } else {
            if (gamma == 1) {
                actual <- actual + m
                fitted.values <- (fitted.values) - m
                future <- (future) - m
                upper.future <- (upper.future) - m
                lower.future <- (lower.future) - m
            } else {
                if (gamma < 0) {
                  actual <- (wqm.Uminus(actual))^(1/gamma) - m
                  fitted.values <- (wqm.Uminus((fitted.values)))^(1/gamma) - m
                  future <- (wqm.Uminus(future))^(1/gamma) - m
                  upper.future <- (wqm.Uminus(upper.future))^(1/gamma) - m
                  lower.future <- (wqm.Uminus(lower.future))^(1/gamma) - m
                } else {
                  if (gamma > 0) {
                    actual <- (actual)^(1/gamma) - m
                    fitted.values <- (fitted.values)^(1/gamma) - m
                    future <- (future)^(1/gamma) - m
                    upper.future <- (upper.future)^(1/gamma) - m
                    lower.future <- (lower.future)^(1/gamma) - m
                  }
                }
            }
        }
        {
            close.screen(all.screens = TRUE)
            split.screen(fig2)
            screen(1)
            par(mar = c(5.1, 4.1, 9.1, 2.1))
            
            if (missing(x.range)) {
                x.range <- range(tsp(actual)[c(1, 2)], tsp(fitted.values)[c(1, 2)], tsp(future)[c(1, 2)], 
                  tsp(lower.future)[c(1, 2)], tsp(upper.future)[c(1, 2)], na.rm = TRUE)
            }
            if (missing(y.range)) {
                y.range <- range(actual, fitted.values, future, lower.future, upper.future, na.rm = TRUE)
            }
            # print(list(x.range = x.range, y.range = y.range))
            
            par(mai = c(0.4, 0.5, 0.7, 0.1))
            plot(actual, ylim = y.range, xlim = x.range, lwd = 1, ylab = response.units, type = "b", main = "", 
                pch = 20)
            mtext(text = "Actual Values, Fitted Values and Predictions with 95% Prediction Intervals", 
                side = 3, line = 2, cex = 1, font = 2, outer = FALSE)
            trans.string <- paste("w=", trans.ylab)
            mtext(text = data.title, side = 3, line = 1, cex = 1.5, font = 2, outer = FALSE)
            mtext(text = paste(model.string, "on", trans.string, xregstr), side = 3, line = 0.2, outer = FALSE)
            lines(actual, lwd = 2)
            lines(fitted.values, col = 2)
            points(future, col = 1, pch = 20)
            lines(lower.future, col = 4)
            
            lines(upper.future, col = 4)
            cat("Forecasts\n")
            forecast.matrix <- cbind(lower.future, future, upper.future)
            dimnames(forecast.matrix) <- list(as.character(seq(1:length(future))), c("Lower", "Forecast", 
                "Upper"))
            if (print.table) 
                print(forecast.matrix)
            screen(2)
            par(pty = "s")
            
            # datax = TRUE, caused problems
            par(mai = c(0.4, 0.5, 0.7, 0.1))
            qqnorm(as.vector(the.residuals), plot = TRUE, main = "Normal Q-Q Plot", xlab = "Normal Scores", 
                ylab = "Residuals", datax = TRUE)
            close.screen(all.screens = TRUE)
            if (!is.null(ps2)) {
                dev.off()
            }
            invisible(arima.out)
        }
    }
    if (!print.table) 
        cat("Use the agrument ',print.table = TRUE' to print a table of the predictions\n\n")
    invisible(arima.out)
}
