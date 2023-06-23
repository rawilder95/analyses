library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)

# load data
dat <- fread('fluencydata_cleaned062223.csv')
dat[, listnum := factor(listnum)]
dat[, age := factor(age)]
dat[, condition := factor(condition)]
dat[, prolific_id := factor(prolific_id)]

# 14 people with no list 2
bad_ids <- dat[,.N,by=.(prolific_id,listnum)][,.N,by=prolific_id][N==1][,prolific_id]
dat <- dat[!(prolific_id %in% bad_ids)]
# 1 person with multiple rows per response (why?)
dat <- unique(dat)
dat[, N:=.N, by=.(prolific_id, listnum, items)][max(N)]
# dat[N>1 & itemnum==max(itemnum), items:= NaN, by= .(prolific_id, listnum,items)]

datdat_n <- dat[,.N,by=.(prolific_id,age,listnum,condition)]
plotdat <- dat_n[,.(N=mean(N)),by=.(age,condition,listnum)]

a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
grid.arrange(a,b)

ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)