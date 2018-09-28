get.trend.prediction <- function(predict.Arima.out, model, the.mean) {
    length.pred <- length(predict.Arima.out$pred)
    d <- model$local["d"]
    seasonal.part <- model$seasonal
    D <- seasonal.part$order["D"]
    period <- seasonal.part$period
    number.zeros <- d
    if (D > 0) 
        number.zeros <- number.zeros + period^D
    number.means <- length.pred - number.zeros
    trend <- rep(the.mean, number.means)
    if (d > 0) {
        for (i in 1:d) {
            trend <- cumsum2(trend, 1, start = 0)
        }
    }
    if (D > 0) {
        for (i in 1:D) {
            trend <- cumsum2(trend, period, start = rep(0, period))
        }
    }
    trend
}
