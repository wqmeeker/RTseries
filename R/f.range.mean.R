

#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics title
f.range.mean <- function(y.ts, period, my.title = NULL) {
    y <- y.ts
    group <- floor(seq(1:(length(y)))/period - 0.01) + 1
    split.list <- split(y, group)
    range.vec <- rep(0, length(split.list))
    mean.vec <- rep(0, length(split.list))
    j <- 0
    # print(c(j))
    for (i in 1:length(split.list)) {
        part <- split.list[[i]]
        if (length(part) < 3) 
            next
        j <- j + 1
        range.hold <- range(part)
        range.vec[j] <- range.hold[2] - range.hold[1]
        mean.vec[j] <- mean(part)
    }
    length(range.vec) <- j
    length(mean.vec) <- j
    mean <- mean.vec
    range <- range.vec
    # browser()
    if (length(mean) == 0) {
        warning(paste("Length of mean is 0; check period =", period, "\n"))
        return(cbind(mean, range))
        
    } else {
        plot(mean, range, type = "n", xlab = "Mean", ylab = "Range")
        if (is.null(my.title)) 
            my.title <- "Range-Mean Plot"
        title(main = my.title)
        points(mean, range, pch = 16, col = 2)
    }
    return(cbind(mean, range))
}
