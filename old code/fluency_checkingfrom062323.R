rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
# library(ezANOVA)
library(cowplot)
<<<<<<< Updated upstream
source('bbar.r')
<<<<<<< HEAD
<<<<<<< HEAD
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# Clean everything up and take out old code that doesn't do anything

# We know this data works
#perseverations
dat= fread('fluency_data_07152023.csv')
perseverations= fread("perseverative_data.csv")
# mean perseverations = 0.013
toplot= perseverations[, mean(perseveration), by= .(age, listnum, condition)]
p1= ggplot(data= toplot[listnum== 1], (aes(x= age, y= V1, fill= condition)))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= "Perseverations List 1")+labs(title= "Perseverations List 1", y= "Proportion of Errors", x= "Age", fill= "Delay Condition")
p2= ggplot(data= toplot[listnum== 2], (aes(x= age, y= V1, fill= condition)))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= "Perseverations List 2", y= "Proportion of Errors", x= "Age", fill= "Delay Condition")
plot_grid(p1,p2)
ggsave('perseverations', device= 'png', dpi= 300)
=======
setwd('~/Downloads/jatos_mac_java/analyses')

=======
setwd('~/Downloads/jatos_mac_java/analyses')

>>>>>>> parent of 55f845a (cluster switches)
=======
setwd('~/Downloads/jatos_mac_java/analyses')
# source('bbar.r')
# d38afbc4ffb1314100d63cda757d9dbfd023f925
>>>>>>> Stashed changes
# load data
d1 <- fread('cleanfluency.csv')
d1[, listnum:= gamenum]
d1= subset(d1, select= -c(gamenum))
>>>>>>> parent of 55f845a (cluster switches)

anova_perseveration= perseverations[, mean(perseveration), by= .(prolific_id, age, condition, listnum)]

anova_perseveration[, prolific_id:= as.factor(prolific_id)]
anova_perseveration[, listnum:= as.factor(listnum)]
anova_perseveration[, condition:= as.factor(condition)]
anova_perseveration[, age:= as.factor(age)]
ezANOVA(data= anova_perseveration, within= "listnum", between=c("prolific_id","age","condition"), dv= V1, wid=prolific_id)

# Repeats
dat[is_repeat== "TRUE" & listnum== 1, is_repeat:= as.logical(0)]
dat[, is_repeat:= as.numeric(is_repeat)]
dat[is_repeat== "TRUE", proportion_repeat:= mean(is_repeat), by= .(prolific_id, listnum, condition, age)]
toplot= dat[listnum==2]
ggplot(data= toplot, aes(x= age, y= proportion_repeat, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= 'Proportion Repeat Trial 2', x= 'Age', y= 'Proportion Repeats', fill= 'Delay Condition')

<<<<<<< HEAD
anova_repeat= dat[listnum== 2, proportion_repeat:= mean(is_repeat), by= .(age,condition,prolific_id)]
anova_repeat= anova_repeat[listnum==2,]
anova_repeat[, listnum:= as.factor(listnum)]
anova_repeat[, age:= as.factor(age)]
anova_repeat[, prolific_id:= as.factor(prolific_id)]
ezANOVA(data= anova_repeat, between=c("age","condition"), dv=proportion_repeat, wid=prolific_id)



=======
demographics= rbind(immediatedemo, delayeddemo)

# merge demographics with fluency data
dat= subset(merge(d1, demographics, by=c("prolific_id", "condition"), all.x=TRUE))

# add age group
dat[ageText<60 & ageText> 25, age:= "WRONG AGE"]
dat[ageText<= 25, age:= "young"]
dat[ageText>=60, age:= "old"]

# get rid of participants that violated age requirements
dat <- dat[!(age == "WRONG AGE"),]

# @Rebecca these participants did not start naming animals for >30s, grounds for exclusion?
dat<-dat[!(prolific_id=="5b33a01fa8327d0001003821" | prolific_id=="5b33a01fa8327d0001003821")]

# @Rebecca in calculating the one_trialers (below), i noticed all of the participants who are removed have List 2 but *not* List 1, which doesn't make sense.
# I checked the starttime and noticed that the list numbers are reversed for all participants in the Delayed condition
# Please fix in the original data file and then delete the three lines below
dat[condition=="Delayed" & listnum == 2, listnum := 3]  # these three lines swap listnums for participants in the delayed condition
dat[condition=="Delayed" & listnum == 1, listnum := 2]
dat[condition=="Delayed" & listnum == 3, listnum := 1]

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

# @Rebecca 7 participants did not repeat any items, and 6 of these are from the Immediate condition. it seems likely that they misunderstood the instructions
# maybe we should not inlcude these participants? 
dat_repeat<-dat_repeat[proportion_repeat != 0.0] 

dat_repeat[,mean(proportion_repeat), by=.(age,condition)] # raw numbers
ezANOVA(dat_repeat, between=c("age","condition"), dv=proportion_repeat, wid=prolific_id)

# plot the data number of repeats by age and condition
bbar(dat_repeat[, proportion_repeat, by=.(age, condition)]) 





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
>>>>>>> parent of 55f845a (cluster switches)
