print.random.model <- function(model) {
    model.names <- names(model)
    if (length(model) == 2) {
        model.string <- paste("ARMA(1,1) with phi_1 =", model$ar[1], "and theta_1 =", model$ma[1])
    } else {
        if (model.names[1] == "ar") {
            ar.part <- model$ar
            ar.order <- length(ar.part)
            if (ar.order == 1) {
                model.string <- paste("AR(1) with phi_1 =", model$ar[1])
            } else {
                model.string <- paste("AR(2) with phi_1 =", model$ar[1], "and phi_2 =", model$ar[2])
            }
        } else {
            ma.part <- model$ma
            ma.order <- length(ma.part)
            if (ma.order == 1) {
                model.string <- paste("MA(1) with theta_1 =", model$ma[1])
            } else {
                model.string <- paste("MA(2) with theta_1 =", model$ma[1], "and theta_2 =", model$ma[2])
            }
        }
    }
    print(model.string, quote = "FALSE")
    # browser()
}
