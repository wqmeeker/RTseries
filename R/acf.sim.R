
#' @importFrom stats acf
acf.sim <- function(sample.size = 30, model = list(ar = 0.9)) {
    sts <- my.arima.sim(model = model, n = sample.size)
    my.acf.plot(acf(sts, plot = F), sample.size = sample.size)
    invisible()
}
