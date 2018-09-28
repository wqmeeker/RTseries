

#' @importFrom stats pchisq
ljung.box <- function(r, n, nparm) {
    K <- min(38 + nparm, length(r) - 1, n - 1)
    ivec <- 1:K
    sum.r2 <- n * (n + 2) * cumsum((1/(n - ivec)) * (r[2:(K + 1)])^2)
    takeoff <- max(5, nparm + 2)
    bl.matrix <- cbind(ivec - nparm, sum.r2)[wqm.Uminus(1:takeoff), ]
    bl.matrix <- cbind(bl.matrix, 1 - pchisq(bl.matrix[, 2], bl.matrix[, 1]))
    dimnames(bl.matrix) <- list(rep("", length = nrow(bl.matrix)), c("dof", "Ljung-Box", "p-value"))
    return(bl.matrix)
}
