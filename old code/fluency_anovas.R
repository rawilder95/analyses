rm(list= ls())
library(data.table)
library(ez)
library(readr)
library(gridExtra)
# Define the which_condition function
# make a list of bad participants 
# did not come back, listed wrong items, did not follow return error.
# SCREEN THIS PARTICIPANT OUT fluency_data[prolific_id== "5c6ae4c31b9d0e000190bacc", prolific_id]
# find unique items not in not in unique scheme items
# see if there's a misspelling in the spellfile
# unique(items) not in unique (scheme items)
# take those items not in scheme and spellcheck
# find replace
# add new spelling errors to scheme 
setwd("~/Downloads/jatos_mac_java/analyses")
dat= fread('cleanfluency.csv')
dat[, listnum:= gamenum]
dat= subset(dat, select= -c(gamenum))
dat[, perseverative:=.N, by= .(prolific_id, items, listnum)]
dat[perseverative==max(perseverative)]
bad_ids <- dat[,.N,by=.(prolific_id,listnum)][,.N,by=prolific_id][N==1][,prolific_id]
dat <- dat[!(prolific_id %in% bad_ids)]
# 1 person with multiple rows per response (why?)
dat <- unique(dat)
demographics= rbind(immediate_demo,subset(delayed_demo, select= -c(V1)))
dat= subset(merge(dat, demographics), select= -c(genderText))



# dat[perseverative>1, max(itemnum), by= .(prolific_id,items, gamenum)][,items:= NaN]
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
# load data
# dat <- fread('fluencydata_cleaned062223.csv')
dat[, listnum := factor(listnum)]
dat[, age := factor(age)]
dat[, condition := factor(condition)]
dat[, prolific_id := factor(prolific_id)]
# 14 people with no list 2
bad_ids <- dat[,.N,by=.(prolific_id,listnum)][,.N,by=prolific_id][N==1][,prolific_id]
dat <- dat[!(prolific_id %in% bad_ids)]
# 1 person with multiple rows per response (why?)
dat <- unique(dat)
demographics= rbind(immediate_demo,subset(delayed_demo, select= -c(V1)))
d1= subset(merge(dat, demographics), select= -c(genderText))
d1[, listnum:= gamenum]
d1= subset(d1, select= -c(gamenum))




dat_n <- dat[,.N,by=.(prolific_id,age,listnum,condition)]
plotdat <- dat_n[,.(N=mean(N)),by=.(age,condition,listnum)]

a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
grid.arrange(a,b)
# ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)
immediate_demo= fread('immediate_demographics.csv')
delayed_demo= fread('delayed_demographics.csv')


d1[,.N,by=.(prolific_id,age,listnum,condition)]



delayed_demo[, condition:= "Delayed"]
delayed_demo= subset(delayed_demo, select= -c(V1))
immediate= merge(immediate, immediate_demo)
# have to do this manually for some f**king reason
delayed= merge(delayed, delayed_demo)
delayedcols= colnames(delayed)
get_names= delayedcols[duplicated(colnames(delayed))]
delayed[, condition:= "Delayed"]
dat= rbind(immediate, delayed)
dat= subset(dat, select= -c(genderText))
dat[ageText>= 60, age:= "old"]
dat[ageText<=25, age:= "young"]
# get ages of participants
wronge_ageids= dat[((ageText>25) & (ageText<60))]$prolific_id
dat= dat[!((ageText>25) & (ageText<60))]
dat[, listnum:= as.character(gamenum)]
dat= subset(dat, select= -c(gamenum))
d1= dat[, .N, by= .(prolific_id, age, condition, listnum)]
d2= d1[, mean(N), by= .(age, condition, listnum)]
d2[, mean(V1), by= age]
d2[, mean(V1), listnum]
d2[, mean(V1), condition]

# go find out if these participants need trials matched
# delayed[, .N, by= .(prolific_id, gamenum)][, .N, by= prolific_id][N<2]$id
# dat[, repwords:= duplicated(items),by= (prolific_id, )]
fwrite(dat, 'fluencydata_cleaned062223.csv')

dat[, listnum:= factor(listnum)]
dat[, listnum:= factor(age)]
dat[, listnum:= factor(condition)]
dat[, listnum:= factor(prolific_id)]
plotdat = dat [,.N , by= .(prolific_id, age,condition, listnum)][, mean(N), by= .(age, condition, listnum)]


p1= ggplot(plotdat[condition== "Immediate"], aes(x= listnum, y= V1))+ geom_bar(aes(fill= age), stat= "identity", position= "dodge")
p2= ggplot(plotdat[condition== "Delayed"], aes(x= listnum, y= V1))+ geom_bar(aes(fill= age), stat= "identity", position= "dodge")




# anova list length
av_ll= ezANOVA(d1, wid = prolific_id, between = c("age", "condition"), within = listnum, dv = N, type= 3)
# ll= av_ll$ANOVA
# 
# d2= as.data.table(ll)
# ggplot(d2, aes(x = Effect, y = F, fill = as.character(p<=0.05))) +
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin = F - 2 * sqrt(F), ymax = F + 2 * sqrt(F)), width = 0.4) +
#   labs(x = "Effect", y = "F-value", fill = "p-value") +
#   theme_minimal()
# ggsave("barplot_anovalistlength.png", device= "png", dpi= 300)
# 
# repwords= dat[,unique(items)]
# 
# d3= merge(dat[, .N, by= .(prolific_id, items)], dat)
# # so we don't want to just say for list 2 (that would be perseverations), we want to grab everything from list 1 and subtract 1
# d4= d3[listnum==2, sum(N-1), by= .(prolific_id, age, condition, listnum)]
# av_rep= ezANOVA(d4, wid = prolific_id, between = c("age", "condition"), dv = V1, type= 3)
# av_rep1= av_rep$ANOVA
# ggplot(av_rep1, aes(x = Effect, y = F, fill = as.character(p<=0.05))) +
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin = F-sqrt(ges), ymax = F+(ges)^2, width = 0.4)) +
#   labs(x = "Effect", y = "F-value", fill = "p-value sig?") +
#   theme_minimal()
# ggsave("barplot_anovarepetitions.png", device= "png", dpi= 300)

