ccor <- function(vcv) {
    if (length(vcv) == 1) {
        cor.mat <- matrix(1)
    } else {
        cor.mat <- diag(1/sqrt(abs(diag(vcv)))) %*% vcv %*% diag(1/sqrt(abs(diag(vcv))))
    }
    dimnames(cor.mat) <- dimnames(vcv)
    return(cor.mat)
}
