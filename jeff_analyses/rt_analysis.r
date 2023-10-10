library(data.table)
library(ez)
library(gridExtra)
source("misc/basicplot.r")

dat <- fread("data/results_noperseveration.csv")
dat[, perseveration := NULL]
dat[, listnum := factor(listnum)]

rtdat <- dat[, .(meanrt = mean(rt), meanlogrt=mean(log(rt))), by=.(age_group, listnum, condition, id)]

# only age_group significant, whether rt or log(rt)

# data (both rt and log rt)
rtdat[,mean(meanrt), by=.(condition, age_group, listnum)]
rtdat[,mean(meanlogrt), by=.(condition, age_group, listnum)]

# inferential
ezANOVA(wid=id, data=rtdat, between=c("age_group","condition"), within=listnum, dv=meanrt)
ezANOVA(wid=id, data=rtdat, between=c("age_group","condition"), within=listnum, dv=meanlogrt)

# plot
a <- bbar(item_level[age_group == "young",num_responses, by=.(condition, listnum)]) + ylim(0,30) + ggtitle("YOUNG")
b <- bbar(item_level[age_group == "old",num_responses, by=.(condition, listnum)]) + ylim(0,30) +ggtitle("OLD")
grid.arrange(a, b, nrow=1)

bbar(item_level[,num_responses, by=.(condition, listnum)])


# age_group significant

