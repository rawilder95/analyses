rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
setwd('~/Downloads/jatos_mac_java/analyses')
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
dat[N>1 & itemnum, perseveration:= 1]
dat[N<=1, perseveration:= 0]
dat[perseveration==1 ,min(itemnum), by= .(prolific_id, listnum, items)][, perseveration:= 0]
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
# Get rid of people who only did one trial; Including Immediate
one_trialers= dat[, unique(listnum), by= prolific_id][, sum(V1), by= prolific_id][V1<3,prolific_id]
dat= dat[!prolific_id %in%one_trialers,]
plot_dat <- dat[, .N, by= .(prolific_id, listnum, age, condition)]
a <- ggplot(plot_dat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill= as.character(listnum))) + geom_bar(stat="identity",position="dodge") + ylim(0,75) + ggtitle("Delayed")+ labs(x= "Age Group", y= "N Items Listed", fill= "Trial")
b <- ggplot(plot_dat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=as.character(listnum))) + geom_bar(stat="identity",position="dodge") + ylim(0,75) + ggtitle("Immediate")+ labs(x= "Age Group", y= "N Items Listed", fill= "Trial")+ ggtitle("Number of Items Listed")
plot_grid(a,b)




### CHECK ROWS END HERE ###
# Repetitions
toplot <- dat[, .N, by= .(items, prolific_id, age, condition)][, sum(N>1)/length(N), by= .(age, condition)]
ggplot(data= toplot)+ geom_bar(aes(x= age ,y= V1, fill= condition), stat= "identity", position= "dodge")+ labs(x= "Age Group", y= "N Items Repeated", fill= "Task Condition")
ggsave("nrepeated_bar.png", device= 'png', dpi= 300)



# repeat repeat trnaitions 
dat[, N:= .N, by= .(items, prolific_id, age, condition)]
dat[, repeated:= as.numeric(N>1)] #get indices for repeated vals
rr_idx= which(dat[, diff(repeated)]==-1) #index all of the rows that are a repeat repeat transition, i.e. 1-1 should == 0

rr_trials= dat[rr_idx,]
p_rrtrials= rr_trials[, .N/nrow(rr_trials), by= .(condition, age)]
ggplot(p_rrtrials, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= "identity", position= "dodge")+ theme_classic()+ labs(x= "Age Group", y= "Proportion of Trials", title= "Repeat-Repeat Transitions", fill= "Trial Condition")
ggsave("prepeatedrepeat_bar.png", device= 'png', dpi= 300)

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
dat_n <- dat[,.N,by=.(prolific_id,age,listnum,condition)]
plotdat <- dat_n[,.(N=mean(N)),by=.(age,condition,listnum)]

a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
grid.arrange(a,b)

ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)



dat= dat[prolific_id %in% dat[, .N, by= .(prolific_id, listnum)][, .N, by= prolific_id][N==2,prolific_id],]
fwrite(dat, 'fluency_data_07152023.csv')

