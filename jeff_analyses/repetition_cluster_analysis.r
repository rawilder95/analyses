library(data.table)
library(ez)
library(ggplot2)
source('repetition_analysis.r')
source("misc/utils.r")

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

# data

anovadat[!is.na(mean_repeated_cluster_size),.(mean_repeated_cluster_size = mean(mean_repeated_cluster_size)), by=.(age_group, condition)]
anovadat[!is.na(mean_new_cluster_size),.(mean_new_cluster_size = mean(mean_new_cluster_size)), by=.(age_group, condition)]

# inferenetial

ezANOVA(anovadat[!is.na(mean_repeated_cluster_size)], wid=id, between=c("age_group","condition"), dv=mean_repeated_cluster_size)
ezANOVA(anovadat[!is.na(mean_new_cluster_size)], wid=id, between=c("age_group","condition"), dv=mean_new_cluster_size)

# plots

bbar(anovadat[!is.na(mean_repeated_cluster_size),mean_repeated_cluster_size, by=.(age_group, condition)])
bbar(anovadat[!is.na(mean_repeated_cluster_size),mean_new_cluster_size, by=.(age_group, condition)])

#anovadat[,mean(num_repeated_cluster_switches), by=.(age_group, condition)]
#ezANOVA(anovadat, wid=id, between=c("age_group","condition"), dv=num_repeated_cluster_switches)

