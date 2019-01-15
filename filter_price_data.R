del_no_vol <- function(pricedata) {
    pricedata$has_vol <- pricedata$volumefrom > 0 | pricedata$volumeto > 0
    idx <- which.max(pricedata$has_vol)
    return(pricedata[idx:nrow(pricedata),])
}