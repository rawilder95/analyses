library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
# load data
### CHECK ROWS FOR DUPLICATES
d1 <- fread('cleanfluency.csv')
d1[, listnum:= gamenum]
d1= subset(d1, select= -c(gamenum))
immediatedemo= fread('immediate_demographics.csv')
delayeddemo= subset(fread('delayed_demographics.csv'), select= -c(V1))
delayeddemo[, condition:= "Delayed"]
demographics= rbind(immediatedemo, delayeddemo)
dat= subset(merge(d1, demographics), select= -c(genderText))
dat= unique(dat)
dat[, N:= .N, by= .(prolific_id, listnum, items)]
dat[N>1, perseveration:= 1]
dat[N<=1, perseveration:= 0]
with_perseveration= dat
dat= dat[perseveration==0,]
# add age
dat[ageText<60 & ageText> 25, age:= "WRONG AGE"]
dat[ageText< 25, age:= "young"]
dat[ageText>60, age:= "old"]
# get rid of participants that violated age requirements
dat <- dat[!(age == "WRONG AGE"),]
# Get n perseverative errors
nperseveration= nrow(with_perseveration) - nrow(dat) #202
dat[, .N, by= .(prolific_id, items, listnum,itemnum)][, max(N)] #max should be 1
# nrow 7800
a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
grid.arrange(a,b)
### CHECK ROWS END HERE ###





dat[, listnum := factor(listnum)]
dat[, age := factor(age)]
dat[, condition := factor(condition)]
dat[, prolific_id := factor(prolific_id)]
# 14 people with no list 2
bad_ids <- dat[,.N,by=.(prolific_id,listnum)][,.N,by=prolific_id][N==1][,prolific_id]
dat <- dat[!(prolific_id %in% bad_ids)]
# 1 person with multiple rows per response (why?)
dat <- unique(dat)
dat[, N:=.N, by=.(prolific_id, listnum, items)][N>1,items:= NaN, by= .(prolific_id, listnum, items)]
dat[N>1 & !(itemnum== min(itemnum)), items:= "PERSEVERATION", by= .(prolific_id, listnum,items)]
datdat_n <- dat[,.N,by=.(prolific_id,age,listnum,condition)]
plotdat <- dat_n[,.(N=mean(N)),by=.(age,condition,listnum)]

a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
grid.arrange(a,b)

ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)