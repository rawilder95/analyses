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
lagcrp_data <- data.table(lag=0, crp=NA, condition=NA, age_group=NA)
for (idx in unique(dat[, id])) {
    age_group <- dat[id == idx, first(age_group)]
    condition <- dat[id == idx, first(condition)]
    recall_list <- dat[id == idx & listnum == 2, item]
    encoding_list <- dat[id == idx & listnum == 1, item]
    new_lagcrp <- lag_crp(encoding_list, recall_list)
    if (nrow(new_lagcrp) > 0) {
        new_lagcrp[, age_group := age_group]
        new_lagcrp[, condition := condition]
    }
    lagcrp_data <- rbind(lagcrp_data, new_lagcrp)
}
crp_all <- summarySE(lagcrp_data, measurevar="crp", groupvars="lag")

# crp separated by condition
crp_immediate <- summarySE(lagcrp_data[condition=="Immediate"], measurevar="crp", groupvars="lag")
crp_delayed <- summarySE(lagcrp_data[condition=="Delayed"], measurevar="crp", groupvars="lag")

crp_immediate <- data.table(crp_immediate)
crp_delayed <- data.table(crp_delayed)
crp_immediate[,condition := "Immediate"]
crp_delayed[,condition := "Delayed"]
crp_by_condition <- rbind(crp_immediate, crp_delayed)

# data
summary_dat[,listnum := factor(listnum)]
summary_dat <- summary_dat[!is.na(tcs)] # need to remove people who dont have a temporal clustering score
summary_dat <- summary_dat[listnum == 2] # only list 2
summary_dat[,mean(tcs,na.rm=T),by=.(condition, age_group)]

# inferential
ezANOVA(dat=summary_dat, wid=id, dv=tcs, between=c("age_group","condition"))

# plot
bbar(summary_dat[,tcs,by=.(age_group,condition)])

    #overall
ggplot(crp_all, aes(x=lag, y=crp)) +
    geom_errorbar(width=.1, aes(ymin=crp-se, ymax=crp+se)) +
    geom_point() + scale_x_continuous(limits=c(-10,10), breaks=-10:10) + theme_classic((base_size = 20)) + xlab("Lag") + ylab ("Conditional Response Probability") 

    # by condition
ggplot(crp_by_condition, aes(color=condition, x=lag, y=crp)) +
    geom_errorbar(width=.1, aes(ymin=crp-se, ymax=crp+se)) +
    geom_point() + scale_x_continuous(limits=c(-10,10), breaks=-10:10) + theme_classic((base_size = 20)) + xlab("Lag") + ylab ("Conditional Response Probability") 
    
