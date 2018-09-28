"param.model1" <- function(x.model, param1, iparam1, debug = F) {
    # fix up model and get other stuff for arima likelihood plot
    if (!is.list(x.model[[1]])) 
        x.model <- list(x.model)
    parameter.start <- 1
    for (i in 1:length(x.model)) {
        model.comp <- x.model[[i]]
        # do the ar side
        if (!is.null(model.comp$period) && model.comp$period > 1) {
            period.string <- paste(model.comp$period, "*", sep = "")
        }
        # do the ma side
        if (length(model.comp$ar.opt) > 0) {
            # get the limits of the indices of these parameters in the overall list
            parameter.end <- parameter.start + length(model.comp$ar.opt) - 1
            if (param1[[1]] >= parameter.start && param1[[1]] <= parameter.end) {
                number.in1 <- param1[[1]] - parameter.start + 1
                labeli <- paste(paste("ar(", period.string, number.in1, sep = ""), ")", sep = "")
                x.model[[i]]$ar.opt[number.in1] <- F
                x.model[[i]]$ar[number.in1] <- param1[[2]][iparam1]
            }
            parameter.start <- parameter.end + 1
        }
        if (length(model.comp$ma.opt) > 0) {
            # get the limits of the indices of these parameters in the overall list
            parameter.end <- parameter.start + length(model.comp$ma.opt) - 1
            if (param1[[1]] >= parameter.start && param1[[1]] <= parameter.end) {
                number.in1 <- param1[[1]] - parameter.start + 1
                labeli <- paste(paste("ma(", period.string, number.in1, sep = ""), ")", sep = "")
                x.model[[i]]$ma.opt[number.in1] <- F
                x.model[[i]]$ma[number.in1] <- param1[[2]][iparam1]
            }
            parameter.start <- parameter.end + 1
        }
    }
    if (debug) 
        browser()
    return(list(model = x.model, nparm = parameter.end, param1.label = labeli))
}

