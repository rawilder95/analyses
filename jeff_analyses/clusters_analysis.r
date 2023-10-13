library(data.table)
library(ez)
library(gridExtra)
source("misc/basicplot.r")

dat <- fread("data/results_noperseveration.csv")
dat[, perseveration := NULL]
dat[, listnum := factor(listnum)]
summary_dat <- dat[,.(num_responses = .N), by=.(id, condition, age_group, listnum)]

# merge with clustering data
clusters <- fread("data/clustering.csv")
clusters[,listnum := factor(listnum)]
setkey(clusters, id, listnum)
setkey(summary_dat, id, listnum)
summary_dat <- merge(summary_dat, clusters)

# data
summary_dat[, .(num_cluster_switches=mean(num_cluster_switches), avg_cluster_size=mean(avg_cluster_size)), by=.(condition, age_group, listnum)]

# inferential
ezANOVA(data=summary_dat, wid=id, between=c("age_group","condition"), within=listnum, dv=num_cluster_switches)
ezANOVA(data=summary_dat, wid=id, between=c("age_group","condition"), within=listnum, dv=avg_cluster_size)

# plot
a <- bbar(summary_dat[age_group == "young", avg_cluster_size, by=.(condition, listnum)]) + ggtitle("YOUNG")
b <- bbar(summary_dat[age_group == "old", avg_cluster_size, by=.(condition, listnum)]) + ggtitle("OLD")
grid.arrange(a, b, nrow=1)



#delta_dat <- dcast(summary_dat, id + condition + age_group ~ listnum, value.var="avg_cluster_size")
#setnames(delta_dat,"1","l1")
#setnames(delta_dat,"2","l2")
#delta_dat[,cluster_size_delta := l2 - l1]
#
#delta_dat[,.(cluster_size_delta = mean(cluster_size_delta)),by=.(age_group, condition)]
#ezANOVA(delta_dat, wid=id, between=c("condition","age_group"), dv=cluster_size_delta)
#bbar(delta_dat[,cluster_size_delta, by=.(age_group, condition)])
