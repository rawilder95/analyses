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
        if (!is.na(new_lag)) { cat(new_lag,"\n") }
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

library(data.table)
library(ez)
library(gridExtra)
source("misc/basicplot.r")

dat <- fread("data/results_noperseveration.csv")
dat[, perseveration := NULL]
dat[, listnum := factor(listnum)]

# find temporal clustering score for each participant
summary_dat <- dat[,.N, by=.(id, listnum, age_group, condition)]
for (idx in unique(summary_dat[,id])) {
    recall_list <- dat[id == idx & listnum == 2, item]
    encoding_list <- dat[id == idx & listnum == 1, item]
    tcs_i <- temporal_clustering_score(encoding_list, recall_list)
    summary_dat[id==idx, tcs := tcs_i]
}

# get data for plotting lag-crp
lagcrp_data <- data.table(lag=0, crp=NA)
for (idx in unique(dat[, id])) {
    cat(idx,"\n")
    recall_list <- dat[id == idx & listnum == 2, item]
    encoding_list <- dat[id == idx & listnum == 1, item]
    new_lagcrp <- lag_crp(encoding_list, recall_list)
    lagcrp_data <- rbind(lagcrp_data, new_lagcrp)
}
crp <- summarySE(lagcrp_data, measurevar="crp", groupvars="lag")

# data
summary_dat[,listnum := factor(listnum)]
summary_dat <- summary_dat[!is.na(tcs)] # need to remove people who dont have a temporal clustering score
summary_dat <- summary_dat[listnum == 2] # only list 2
summary_dat[,mean(tcs,na.rm=T),by=.(condition, age_group)]

# inferential
ezANOVA(dat=summary_dat, wid=id, dv=tcs, between=c("age_group","condition"))

# plot
bbar(summary_dat[,tcs,by=.(age_group,condition)])

ggplot(crp[lag >= -4 & lag <= 4], aes(x=lag, y=crp)) +
    geom_errorbar(width=.1, aes(ymin=crp-se, ymax=crp+se)) +
    geom_point() + ylim(0,.5) + scale_x_continuous(limits=c(-10,10), breaks=-10:10) + theme_classic((base_size = 20)) + ylim(0.0,0.3) + xlab("Lag") + ylab ("Conditional Response Probability") 


