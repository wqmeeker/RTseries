

#' @importFrom stats arima.sim
#' @importFrom stats as.ts
my.arima.sim <- function(model, n, rand.gen = rnorm, innov = rand.gen(n, ...), n.start = NA, start.innov = rand.gen(n.start, 
    ...), ...) {
    model.names <- names(model)
    # fix for problem
    if (any(model.names == "ma")) 
        model$ma <- -model$ma
    # print.default(model)
    x <- arima.sim(model = model, n = n)
    as.ts(x)
}
