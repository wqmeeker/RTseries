
#' List Objects of a Given Mode
#' @description
#' Provides a list of objects with a specified mode (functions by default).
#' @param pos Position on the search list to look (default is 1).
#' @param mode Type of object to report; default is function
#' @examples
#' ls.mode()
#' ls.mode(pos=2)
#' ls.mode(mode='tsd')
#' ls.mode(mode='tsd', pos=2)
#'
#' @export
ls.mode <- function(pos = 1, mode = "function") {
    dimnames(wqm.objects.summary(mode = mode, pos = pos))[[1]]
}
