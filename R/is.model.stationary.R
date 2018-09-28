"is.model.stationary" <- function(x.model.list) {
    # check the stationarity/invertability of a specified AR or MA model
    x.model <- x.model.list$model
    # AR part
    for (i in 1:length(x.model)) {
        if (any(names(x.model[[i]]) == "ar")) {
            coefficients <- x.model[[i]]$ar
            if (!is.stationary(coefficients)) 
                return(FALSE)
        }
        # MA part
        if (any(names(x.model[[i]]) == "ma")) {
            coefficients <- x.model[[i]]$ma
            if (!is.stationary(coefficients)) 
                return(FALSE)
        }
    }
    return(TRUE)
}

