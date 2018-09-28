getNumberParameters <- function(model) {
    # get the number of parameters in the time series model
    numberParameters <- model$local["p"] + model$local["q"] + model$seasonal$order["P"] + model$seasonal$order["Q"]
    if (model$local[2] == 0 && model$seasonal$order[2] == 0) {
        numberParameters <- numberParameters + 1
    }
}
