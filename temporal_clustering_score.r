temporal_clustering_score <- function(encoding_list, recall_list) {
    tcss <- c()
    for (i in 2:length(recall_list)) {
        # only if current and next item are not perseverations
        if ((length(which(recall_list[1:i] == recall_list[i])) == 1) && (length(which(recall_list[1:i] == recall_list[i-1])) == 1)) {
            new_lag <- abs(lag(encoding_list, recall_list[i-1], recall_list[i]))
            if (!is.na(new_lag)) {  # only if a lag can be computed (i.e., both items have serial positions)
                possible_new_transitions <- encoding_list[!(encoding_list %in% recall_list[1:i])] # excludes actual transition
                if (length(possible_new_transitions) > 0) { # if there were other transitions that could have been made
                    new_possible_lags <- abs(sapply(possible_new_transitions, function(x) { lag(encoding_list, recall_list[i-1], x) }))
                    new_tcs <- mean(sapply(new_possible_lags, function(x) { 
                                   if (new_lag < x) { return(1) }
                                   if (new_lag == x) { return(0.5) }
                                   if (new_lag > x) { return(0) }
                                }))
                    tcss <- c(tcss, new_tcs) 
                }
            }
        }
    }
    tcs <- mean(tcss)
    return(tcs)
}
