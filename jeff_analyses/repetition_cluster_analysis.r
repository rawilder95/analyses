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


library(data.table)
library(ez)
library(ggplot2)
source('repetition_analysis.r')

item_level_repeats <- item_level_repeats[order(id,listnum,itemnum)] 
list2 <- item_level_repeats[listnum==2]

for (idx in unique(list2[,id])) {
    fluency_list <- list2[id == idx, repeated]
    returnvals <- repeated_clustering_sizes(fluency_list)
    list2[id == idx, mean_new_cluster_size := returnvals[1]]
    list2[id == idx, mean_repeated_cluster_size := returnvals[2]]
    list2[id == idx, num_repeated_cluster_switches := returnvals[3]]
}

anovadat <- unique(list2[,.(id, mean_new_cluster_size, mean_repeated_cluster_size, num_repeated_cluster_switches, age_group, condition)])

anovadat[,mean(num_repeated_cluster_switches), by=.(age_group, condition)]
ezANOVA(anovadat, wid=id, between=c("age_group","condition"), dv=num_repeated_cluster_switches)

anovadat[!is.na(mean_repeated_cluster_size),mean(num_repeated_cluster_switches), by=.(age_group, condition)]
ezANOVA(anovadat[!is.na(mean_repeated_cluster_size)], wid=id, between=c("age_group","condition"), dv=mean_repeated_cluster_size)

anovadat[!is.na(mean_new_cluster_size),mean(num_repeated_cluster_switches), by=.(age_group, condition)]
ezANOVA(anovadat[!is.na(mean_new_cluster_size)], wid=id, between=c("age_group","condition"), dv=mean_new_cluster_size)
