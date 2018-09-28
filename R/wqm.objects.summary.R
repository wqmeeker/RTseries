wqm.objects.summary <- function(pos = 1, mode = "all") {
    
    object.list <- objects(pos = pos)
    if (length(object.list) == 0) {
        return("")
    }
    class.get <- function(str) class(wqm.get(str))[1]
    the.class <- unlist(lapply(object.list, class.get))
    the.frame <- data.frame(the.class)
    rownames(the.frame) <- object.list
    if (mode == "all") {
        return(the.frame)
    } else {
        is.the.mode <- the.frame[, "the.class"] == mode
        if (any(is.the.mode)) {
            return(the.frame[is.the.mode, , drop = F])
        }
    }
    return("")
}
