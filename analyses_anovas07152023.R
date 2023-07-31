rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
setwd('~/Downloads/jatos_mac_java/analyses')

dat= fread('fluency_data_07152023.csv')
# spellcheck again 
# participants that didn't start naming animals for 30 seconds
# N words listed trial by delay by age
# fix the code in the data file 
spellcheck = fread('updatedsnafuspelling.csv')
### SPELLCHECK ###
scheme = fread('updatedsnafuscheme.csv')
dat[! items %in% scheme$word, items]




















d1= dat[ ,.N, by= .(prolific_id, age, condition, listnum)]
d1[, prolific_id:= factor(prolific_id)]
d1[, age:= factor(age)]
d1[, listnum:= factor(listnum)]
d1[, condition:= factor(condition)]
## ANOVA for N items listed
##Warning:Data is unbalanced (unequal N per group).
##Same for listnum= 173, age: old= 180, young= 166, condition: immediate = 186, delayed = 160
#ezANOVA(d1, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)
#
## bar graph for N repeated words

bar_fig1= dat[repeated==1, .N, by= .(prolific_id, age, condition)][, mean(N), by= .(age, condition)]
# plot bar for repeated words
ggplot(data= bar_fig1)+ geom_bar(aes(x= age, y= V1, fill= condition),position= "dodge", stat= "identity")+ labs(x= "Age", y= "N Repeated Words", fill= "Delay")
ggsave('bar_repeatedwords.png', device= 'png', dpi= 300)
d1= dat[repeated==1, .N, by= .(prolific_id, age, condition)]
d1[, age:= factor(age)]
d1[, condition:= factor(condition)]
d1[, prolific_id:= factor(prolific_id)]
ezANOVA(d1, between=c("age","condition"), dv=N, wid=prolific_id)
# 8dbdad66c9d72473e75f9e7fc068bb47529c8e76

# change in response length from t1- t2
list1= dat[listnum==1, .N, by= .(prolific_id, condition, age)]
list2= dat[listnum==2, .N, by= .(prolific_id, condition, age)]

deltachange= list1 #MERGE THESE INSTEAD
deltachange[, N2:= list2$N] # @Rebecca This does not work because the two data tables are not ordered the same. It will work if you first order both by prolific_id, but it's much safer to do a merge. Plot below is not meaningful.
deltachange[, d:= N-N2] 
meandelta= deltachange[, mean(d), by= .(condition, age)]

ggplot(data= meandelta)+ geom_bar(aes(x= age, y= V1, fill= condition), position= 'dodge', stat= 'identity')
ggsave('bar_deltachange.png', device= 'png', dpi= 300)

witherrors= fread("perseverative_data.csv")
witherrors[, meanerror:= mean(perseveration), by= .(prolific_id, age, listnum, condition)]
plot_data <- witherrors[,mean(meanerror), by=.(condition, age)]
setnames(plot_data, "V1", "perseveration_rate")
    
ggplot(data= plot_data) + geom_bar(aes(x= age, y= perseveration_rate, fill= condition), position= 'dodge', stat= 'identity')+ labs(x= 'Age', y= 'Proportion of Errors', fill= "Delay")
ggsave('proportionoferrors.png', device= 'png', dpi= 300)
# immediate= fread('immediatefluency_merged.csv')
# delayed= fread('delayedfluency_merged.csv')
# 

#



ggplot(data= witherrors) + geom_bar(aes(x= age, y= meanerror, fill= condition), position= 'dodge', stat= 'identity')+ labs(x= 'Age', y= 'Proportion of Errors', fill= "Delay")
d1= fread('cleanfluency.csv')
d1[gamenum==2, listnum:= 1]
d1[gamenum==1, listnum:= 2]
ggsave('celebrity_personality.png', device= 'png', dpi= 300)


# load data
d1 <- fread('cleanfluency.csv')
d1[, listnum:= gamenum]
d1= subset(d1, select= -c(gamenum))


# load demographics
immediatedemo= fread('immediate_demographics.csv')
immediatedemo <- immediatedemo[,.(prolific_id, condition, gender, ageText)]

delayeddemo= subset(fread('delayed_demographics.csv'), select= -c(V1))
delayeddemo[, condition:= "Delayed"]
delayeddemo[,first_id := min(jatos_id), by=prolific_id]
delayeddemo <- delayeddemo[jatos_id==first_id]              # the delayed demographics contain two rows for each participant; this selects just the first row per participant
delayeddemo <- delayeddemo[,.(prolific_id, condition, gender, ageText)]

demographics= rbind(immediatedemo, delayeddemo)

# merge demographics with fluency data
dat= subset(merge(d1, demographics, by=c("prolific_id", "condition"), all.x=TRUE))

# add age group
dat[ageText<60 & ageText> 25, age:= "WRONG AGE"]
dat[ageText<= 25, age:= "young"]
dat[ageText>=60, age:= "old"]

# get rid of participants that violated age requirements
dat <- dat[!(age == "WRONG AGE"),]

# these participants did not start naming animals for >30s, grounds for exclusion?
#dat<-dat[!(prolific_id=="5b33a01fa8327d0001003821" | prolific_id=="5b33a01fa8327d0001003821")]

# Get rid of people who only did one trial; Including Immediate
one_trialers= dat[, unique(listnum), by= prolific_id][, sum(V1), by= prolific_id][V1<3,prolific_id]
dat= dat[!prolific_id %in% one_trialers,]

# convert IVs to factors
dat[, listnum := factor(listnum)]
dat[, age := factor(age)]
dat[, condition := factor(condition)]

# mark and remove perseverations
dat[, N:= .N, by= .(prolific_id, listnum, items)]
dat[N>1, perseveration:= 1]
dat[N<=1, perseveration:= 0]

dat[perseveration==1,minitemnum := min(itemnum),by=.(prolific_id, listnum, items)] # note itemnum of first occurrence
dat[perseveration==1 & itemnum==minitemnum, perseveration := 0] # mark first occurrences as not perseverations
dat[,minitemnum := NULL]
dat[,N := NULL]

anova_perseveration= with_perseveration[, mean(perseveration), by= .(prolific_id, listnum, age, condition)]
anova_perseveration[, listnum:= factor(listnum)]


ezANOVA(anova_perseveration, within=listnum, between=c("age","condition"), dv=V1, wid=prolific_id)

# Do the perseveration analysis

with_perseveration= dat # keep data with perseverations for later analysis
# fwrite(with_perseveration, "perseverative_data.csv")

dat= dat[perseveration==0,] # remove perseverations in dat; note: itemnum will no longer be sequential for most participants

# how many perseveartive errors in total?
nperseveration= nrow(with_perseveration) - nrow(dat) # 100

# how many valid responses per participant/list?
dat[,num_valid_responses := .N, by=.(prolific_id, listnum)]

# Q: does number of valid responses differ by age, condition, or listnum?
dat_n <- unique(dat[,.(prolific_id, age, condition, listnum, num_valid_responses)]) # new data table with 1 row per participant/listnum for data analysis
dat_n[,mean(num_valid_responses),by=.(condition,age,listnum)] # raw numbers
ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=num_valid_responses, wid=prolific_id)

# plot the data number of valid responses by age, condition, and listnum
a <- bbar(dat[condition=="Immediate", num_valid_responses, by=.(age, listnum)]) + ggtitle("Immediate") + ylim(0,30)
b <- bbar(dat[condition=="Delayed", num_valid_responses, by=.(age, listnum)]) + ggtitle("Delayed") + ylim(0,30)
plot_grid(a,b)

# mark repeats
dat[, is_repeat := (.N>1), by= .(items, prolific_id, age, condition)]
dat[listnum==2, proportion_repeat := sum(is_repeat)/.N, by= .(prolific_id)]

# Q: does proportion of repeats differ by age or condition?
dat_repeat <- unique(dat[listnum==2,.(prolific_id, age, condition, proportion_repeat)]) # new data table with 1 row per participant (2nd list only)

# 7 participants did not repeat any items, and 6 of these are from the Immediate condition. it seems likely that they misunderstood the instructions
# maybe we should not inlcude these participants? 
#dat_repeat<-dat_repeat[proportion_repeat != 0.0] 

dat_repeat[,mean(proportion_repeat), by=.(age,condition)] # raw numbers
ezANOVA(dat_repeat, between=c("age","condition"), dv=proportion_repeat, wid=prolific_id)

# plot the data number of repeats by age and condition
bbar(dat_repeat[, proportion_repeat, by=.(age, condition)]) 



# Semantic fluency analysis tool
sem_analysis= subset(dat, select= c(prolific_id, items))
colnames(sem_analysis) <- c('subject_id', 'words')
rownames(sem_analysis) <- NULL
write.table(sem_analysis, file = 'sem_analysis.txt', sep = '\t', row.names= FALSE )

# read-in online results 
cluster_switch= subset(fread('sem_analysis_model_none_switch_all_switch_results.csv'), select= -c(V1))
cluster_switch[, itemnum:= 1:nrow(cluster_switch)]
cluster_switch[, prolific_id:= Subject]
cluster_switch= subset(cluster_switch, select= -c(Subject))
# get clusters >1
clusters= cluster_switch[Switch_Value>0]
# Merge with original data
# ncluster= merge(cluster_switch, dat, by= "prolific_id")

# take output and calculate avg cluster size
# put in work streams

dat[prolific_id %in% cluster_switch$prolific_id]$prolific_id




### CHECK ROWS END HERE ###
# Repetitions
#toplot <- dat[, .N, by= .(items, prolific_id, age, condition)][, sum(N>1)/length(N), by= .(age, condition)]
#ggplot(data= toplot)+ geom_bar(aes(x= age ,y= V1, fill= condition), stat= "identity", position= "dodge")+ labs(x= "Age Group", y= "N Items Repeated", fill= "Task Condition")
#ggsave("figs/nrepeated_bar.png", device= 'png', dpi= 300)



# repeat repeat trnaitions 
#dat[, N:= .N, by= .(items, prolific_id, age, condition)]
#dat[, repeated:= as.numeric(N>1)] #get indices for repeated vals
#rr_idx= which(dat[, diff(repeated)]==-1) #index all of the rows that are a repeat repeat transition, i.e. 1-1 should == 0
#
#rr_trials= dat[rr_idx,]
#p_rrtrials= rr_trials[, .N/nrow(rr_trials), by= .(condition, age)]
#ggplot(p_rrtrials, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= "identity", position= "dodge")+ theme_classic()+ labs(x= "Age Group", y= "Proportion of Trials", title= "Repeat-Repeat Transitions", fill= "Trial Condition")
#ggsave("figs/prepeatedrepeat_bar.png", device= 'png', dpi= 300)
#
#dat[, listnum := factor(listnum)]
#dat[, age := factor(age)]
#dat[, condition := factor(condition)]
#dat[, prolific_id := factor(prolific_id)]
## 14 people with no list 2
#bad_ids <- dat[,.N,by=.(prolific_id,listnum)][,.N,by=prolific_id][N==1][,prolific_id]
#dat <- dat[!(prolific_id %in% bad_ids)]
## 1 person with multiple rows per response (why?)
#dat <- unique(dat)
#dat[, N:=.N, by=.(prolific_id, listnum, items)][N>1,items:= NaN, by= .(prolific_id, listnum, items)]
#dat[N>1 & !(itemnum== min(itemnum)), items:= "PERSEVERATION", by= .(prolific_id, listnum,items)]
#dat_n <- dat[,.N,by=.(prolific_id,age,listnum,condition)]
#plotdat <- dat_n[,.(N=mean(N)),by=.(age,condition,listnum)]
#
#a <- ggplot(plotdat[condition=="Delayed"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Delayed")
#b <- ggplot(plotdat[condition=="Immediate"], aes(y=N, x=age, group=listnum, fill=listnum)) + geom_bar(stat="identity",position="dodge") + ylim(0,30) + ggtitle("Immediate")
#grid.arrange(a,b)
#
#ezANOVA(dat_n, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)
#
#
#
#dat= dat[prolific_id %in% dat[, .N, by= .(prolific_id, listnum)][, .N, by= prolific_id][N==2,prolific_id],]
# fwrite(dat, 'fluency_data_07152023.csv')