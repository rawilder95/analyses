# Return the lag for a transition between response1 and response2
# If either response is not in the encoding list, return NA
lag <- function(encoding_list, response1, response2) {
    serial_position1 <- which(encoding_list == response1)
    serial_position2 <- which(encoding_list == response2)
    lag <- (serial_position2 - serial_position1)
    if (length(lag) == 0) { lag <- NA }  # if one item doesn't have a serial position then set lag to NA
    return(lag)
}

# Return lag-CRP for each lag (in the form of a data table) for a single recall list
lag_crp <- function(encoding_list, recall_list) {
    lags <- c()
    possible_lags <- c()
    for (i in 2:length(recall_list)) {
        # only if current and next item are not perseverations
        if ((length(which(recall_list[1:i] == recall_list[i])) == 1) && (length(which(recall_list[1:i] == recall_list[i-1])) == 1)) {
            new_lag <- lag(encoding_list, recall_list[i-1], recall_list[i])
            if (!is.na(new_lag)) {  # only if a lag can be computed (i.e., both items have serial positions)
                lags <- c(lags, new_lag)
                possible_new_transitions <- encoding_list[!(encoding_list %in% recall_list[1:i-1])]
                new_possible_lags <- sapply(possible_new_transitions, function(x) { lag(encoding_list, recall_list[i-1], x) })
                possible_lags <- c(possible_lags, new_possible_lags)
            }
        }
    }
    # now calculate lag-CRP from counts
    possible_lag_tokens <- sort(unique(possible_lags))
    crp <- c()
    for (i in possible_lag_tokens) {
        crp <- tryCatch({
            c(crp, (table(lags)[[as.name(i)]] / table(possible_lags)[[as.name(i)]]))
        }, error = function(e) {
            c(crp, 0)   # i.e., numerator is 0 because there are no observed transitions of this lag length
        })
    }
    list_crp <- data.table(lag=possible_lag_tokens, crp=crp)
    return(list_crp)
}
