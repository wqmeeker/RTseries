difference.series <- function(y, model) {
    if (model$local["d"] > 0) {
        y <- diff(y, differences = model$local["d"], lag = 1)
    }
    seasonal.part <- model$seasonal
    if (is.list(seasonal.part)) {
        if (seasonal.part$order["D"] > 0) {
            y <- diff(y, differences = seasonal.part$order["D"], lag = seasonal.part$period)
        }
    } else {
        stop("Model seasonal part needs to be a list here")
    }
    y
}
