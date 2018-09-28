

#' @importFrom stats tsp
complete.model <- function(model, data.tsd) {
    order <- model$local
    # pick up seasonal period from the data, if needed
    seasonal.part <- model$seasonal
    if (is.list(seasonal.part)) {
        seasonal.period <- seasonal.part$period
        return(model)
    } else {
        period <- tsp(data.tsd)[3]
        # if(period==1)stop('Model without period specified and the data are not seasonal')
        seasonal <- list(order = model$seasonal, period = period)
        new.model <- list(local = order, seasonal = seasonal)
    }
    return(new.model)
}
