
#' @importFrom stats runif
random.model <- function(model.prior = c(0.35, 0.35, 0.3), factor = 1000) {
    dist.vec <- rep(1:length(model.prior), model.prior * factor)
    model.now <- dist.vec[runif(1) * factor + 1]
    r.model <- 888
    if (model.now < 1 || model.now > 3) 
        print(list(dist.vec, model.now))
    # print(list(model.now, r.model))
    switch(model.now, {
        index <- rep(1:nrow(ma.model.prior()), times = (factor * ma.model.prior()[, 3])/sum(ma.model.prior()[, 
            3]))[factor * runif(1) + 1]
        r.model <- list(ma = ma.model.prior()[index, c(1, 2)])
    }, {
        index <- rep(1:nrow(ma.model.prior()), times = (factor * ar.model.prior()[, 3])/sum(ar.model.prior()[, 
            3]))[factor * runif(1) + 1]
        r.model <- list(ar = ar.model.prior()[index, c(1, 2)])
    }, {
        index <- rep(1:nrow(arma.model.prior()), times = (factor * arma.model.prior()[, 3])/sum(arma.model.prior()[, 
            3]))[factor * runif(1) + 1]
        r.model <- list(ma = arma.model.prior()[index, 1], ar = arma.model.prior()[index, 2])
    })
    class(r.model) <- "random.model"
    return(r.model)
}
