library(data.table)
library(ez)
library(gridExtra)
source("misc/basicplot.r")
source("misc/utils.r")

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

ggplot(crp, aes(x=lag, y=crp)) +
    geom_errorbar(width=.1, aes(ymin=crp-se, ymax=crp+se)) +
    geom_point() + scale_x_continuous(limits=c(-10,10), breaks=-10:10) + theme_classic((base_size = 20)) + xlab("Lag") + ylab ("Conditional Response Probability") 


