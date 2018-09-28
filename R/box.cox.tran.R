box.cox.tran <- function(data.dts, gamma, m, ylab = NULL) {
    if (m == 0) {
        char.m <- NULL
    } else {
        char.m <- paste("+", format(m))
    }
    if (gamma == 0) {
        y.trans <- log(data.dts + m)
        trans.string <- paste("log(", ylab, char.m, ")", sep = "")
    } else {
        if (gamma == 1) {
            y.trans <- data.dts + m
            trans.string <- paste(ylab, char.m, sep = "")
        } else {
            y.trans <- (((data.dts + m)^gamma - 1)/gamma)
            trans.string <- paste("((", ylab, char.m, ")", "^(", format(gamma), "))-1)/(", format(gamma), 
                ")", sep = "")
        }
    }
    attr(y.trans, "trans.string") <- trans.string
    return(y.trans)
}
