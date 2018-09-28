bartlett.se <- function(r, n) {
    sum.r2 <- sqrt((1 + 2 * c(0, cumsum((r[wqm.Uminus(length(r))])^2)))/n)
    return(sum.r2)
}
