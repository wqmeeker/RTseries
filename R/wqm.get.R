wqm.get <- function(name, pos = 1, where = 1, inherit = FALSE, mode = "any") {
    if (missing(pos)) {
        get(name)
    } else {
        get(name, pos = pos)
    }
}
