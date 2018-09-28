pause <- function(prompt = "Touch Return to continue; type stop to breakout: ") {
    if (!interactive()) 
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
        if (ans == "stop") 
            stop("Stopping")
    }
}
