#' Gives the y position on a plot for a specified relatrive location.
#' 
#' @param perc.loc A number between 0 and 1.
#' 
#' @export
y.loc <- function(perc.loc) {
    usr.out <- par("usr")
    return(usr.out[3] + perc.loc * (usr.out[4] - usr.out[3]))
}
