#' Gives the x position on a plot for a specified relatrive location.
#' 
#' @param perc.loc A number between 0 and 1.
#' 
#' @export
x.loc <- function(perc.loc) {
    usr.out <- par("usr")
    return(usr.out[1] + perc.loc * (usr.out[2] - usr.out[1]))
}
