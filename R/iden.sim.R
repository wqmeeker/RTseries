#' Model Identification Using Simulated Data
#' @description
#' This function will simulate samples from an ARMA model, plot the data, compute and plot sample ACF and PACF functions (using the \code{\link{iden}} function), and let user gain experience of tentatively identifying a time series model. After each simulation, the user will be prompted to ask if another set of simulated data (from the same model) should be genrated or not. When the answer is no (or n), the model and its parameters are displayed and the user is asked whether a new model should be simulated or not.
#' @param realization.size Size of the sample realization to be generated (default is 100).
#' @param model ARMA model. If no model is specified, a random model will be generated.
#' @param lag.max Maximum lag for the ACF and PACF functions (default is 20).
#' @param plot.true If plot.true = TRUE the function true ACF/PACF functions will display as the model is revealed.
#' @return the last simulated data set is returned invisibly.
#' @examples
#' iden.sim()
#' iden.sim(model=list(ar=c(.9, -.5)))
#' iden.sim(model=list(ar=c(.9,-.5),ma=c(.1,.1)))
#'
#' @export
iden.sim <- function(realization.size = 100, model, lag.max = 20, plot.true = FALSE) {
    
    missing.model <- missing(model)
    repeat {
        if (missing.model) {
            model <- random.model()
            cat("Random model and parameters have been chosen \n")
        }
        repeat {
            data.tsd <- tsd(data.ts = my.arima.sim(n = realization.size, model = model), data.title = "Simulated data", 
                response.units = "Data")
            cat(paste("Realization of length", realization.size, " simulated \n"))
            iden(data.tsd = data.tsd, lag.max = lag.max)
            if (!missing.model || pause("More data? ")) {
                cat("previous model was: \n")
                print(model)
                break
            }
        }
        if (plot.true) 
            show.true.acfpacf(model)
        if (!missing.model || pause("New model? ")) {
            break
        }
    }
    attr(data.tsd, "model") <- model
    invisible(data.tsd)
}
