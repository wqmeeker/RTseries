f.true.model.comp.string <- function(model.comp) {
    string <- ""
    if (!is.null(model.comp$ma)) 
        string <- paste(string, "ma:", paste(model.comp$ma, collapse = ", "), sep = "  ")
    if (!is.null(model.comp$ar)) 
        string <- paste(string, "ar:", paste(model.comp$ar, collapse = ", "), sep = "  ")
    return(string)
}
