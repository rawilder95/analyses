temporal_clustering_score <- function(encoding_list, recall_list) {
    tcss <- c()
    for (i in 2:length(recall_list)) {
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
    tcs <- mean(tcss)
    return(tcs)
}

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
        new_lag <- lag(encoding_list, recall_list[i-1], recall_list[i])
        if (!is.na(new_lag)) {  # only if a lag can be computed (i.e., both items have serial positions)
            lags <- c(lags, new_lag)
            possible_new_transitions <- encoding_list[!(encoding_list %in% recall_list[1:i-1])]
            new_possible_lags <- sapply(possible_new_transitions, function(x) { lag(encoding_list, recall_list[i-1], x) })
            possible_lags <- c(possible_lags, new_possible_lags)
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

repeated_clustering_sizes <- function(fluency_list) {

    new_cluster_sizes <- c()        # sizes for new-item clusters
    repeated_cluster_sizes <- c()   # sizes for repeated-item clusters
    previous_repeatYN <- fluency_list[1]     # is the first item a repeat?
    cluster_size <- 1
    
    for (i in 2:length(fluency_list)) {
        repeatYN <- fluency_list[i]
        if (repeatYN == previous_repeatYN) {
            cluster_size <- cluster_size + 1
        } else {
            if (previous_repeatYN == 0) {
                new_cluster_sizes <- c(new_cluster_sizes, cluster_size)
            } else {
                repeated_cluster_sizes <- c(repeated_cluster_sizes, cluster_size)
            }
            cluster_size <- 1 # reset cluster size on switch

        }
        previous_repeatYN <- repeatYN # keep track of whether previous item was new or old
    }

    # edge case to add last cluster size when fluency list ends. redundant code...
    if (previous_repeatYN == 0) {
        new_cluster_sizes <- c(new_cluster_sizes, cluster_size)
    } else {
        repeated_cluster_sizes <- c(repeated_cluster_sizes, cluster_size)
    }
    
    num_cluster_switches <- sum(length(new_cluster_sizes), length(repeated_cluster_sizes))-1 
    returnvals <- c(mean(new_cluster_sizes), mean(repeated_cluster_sizes), num_cluster_switches)
    return(returnvals) # some values may be NA if participant never repeats items (or only repeats items)
}



