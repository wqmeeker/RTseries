f.model.string <- structure(function(model) {
    local <- model$local
    local.model.string <- paste("(", local[1], ",", local[2], ",", local[3], ")", sep = "")
    seasonal.part <- model$seasonal
    local <- seasonal.part$order
    if (sum(local) == 0) 
        return(paste("ARIMA", local.model.string, sep = ""))
    seasonal.model.string <- paste("(", local[1], ",", local[2], ",", local[3], ")", sep = "")
    seasonal.period <- seasonal.part$period
    return(paste("SARIMA", local.model.string, seasonal.model.string, "_", seasonal.period, sep = ""))
}, source = c("function(model)", "{", "", "  #print(model)", "  #", "  #first do the local part", "  #", 
    "  local <- model$local", "\t\t", "  local.model.string <-paste(\"(\",local[1],\",\",local[2],\",\",local[3],\")\",sep=\"\")", 
    "", "  seasonal.part <- model$seasonal", "  local <- seasonal.part$order", "  if(sum(local)==0)", 
    "    return(paste(\"ARIMA\",local.model.string ,sep=\"\"))", "", "  ", "  seasonal.model.string <-paste(\"(\",local[1],\",\",local[2],\",\",local[3],\")\",sep=\"\")", 
    "  seasonal.period <- seasonal.part$period", "", "", "    return(paste(\"SARIMA\",local.model.string , seasonal.model.string ,\"_\",seasonal.period,sep=\"\"))", 
    "", "", "  ", "      }"))
