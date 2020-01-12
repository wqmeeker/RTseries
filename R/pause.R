#' Allows a pause in the running of an R function.
#'
#' @export
pause <- function(prompt = "Touch Return to continue; type stop to breakout: ", skip = FALSE, 
    browse = FALSE) {
    if (browse) 
        browser()
    if (skip) 
        return(FALSE)
    if (!interactive() || (exists(".Batch") && .Batch)) 
        return(TRUE)
    repeat {
        cat(prompt)
        ans <- readline()
        invisible()
        if (ans == "n" || ans == "no") 
            return(TRUE)
        if (ans == "yes" || ans == "y") 
            return(FALSE)
        if (ans == "") 
            return(FALSE)
        if (ans == "abort") {
            stop("Abort")
            break
        }
        if (ans == "stop") {
            cat("Breaking out \n")
            stop("Abort")
            return(TRUE)
        }
    }
}
