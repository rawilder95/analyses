rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
# setwd('~/Downloads/jatos_mac_java/analyses')

dat= fread('fluency_data_07152023.csv')
# spellcheck again 
# participants that didn't start naming animals for 30 seconds
# N words listed trial by delay by age
# fix the code in the data file 


#d1= dat[ ,.N, by= .(prolific_id, age, condition, listnum)]
#d1[, prolific_id:= factor(prolific_id)]
#d1[, age:= factor(age)]
#d1[, listnum:= factor(listnum)]
#d1[, condition:= factor(condition)]
## ANOVA for N items listed
##Warning:Data is unbalanced (unequal N per group).
##Same for listnum= 173, age: old= 180, young= 166, condition: immediate = 186, delayed = 160
#ezANOVA(d1, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)
#
## bar graph for N repeated words
#bar_fig1= dat[repeated==1, .N, by= .(prolific_id, age, condition)][, mean(N), by= .(age, condition)]
## plot bar for repeated words 
#ggplot(data= bar_fig1)+ geom_bar(aes(x= age, y= V1, fill= condition),position= "dodge", stat= "identity")+ labs(x= "Age", y= "N Repeated Words", fill= "Delay")
#ggsave('bar_repeatedwords.png', device= 'png', dpi= 300)
#d1= dat[repeated==1, .N, by= .(prolific_id, age, condition)]
#d1[, age:= factor(age)]
#d1[, condition:= factor(condition)]
#d1[, prolific_id:= factor(prolific_id)]
#ezANOVA(d1, between=c("age","condition"), dv=N, wid=prolific_id)

# change in response length from t1- t2
list1= dat[listnum==1, .N, by= .(prolific_id, condition, age)]
list2= dat[listnum==2, .N, by= .(prolific_id, condition, age)]

deltachange= list1
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
# immediate[, items:= tolower(items)]
# delayed[, items:= tolower(items)]
# uncleaned= rbind(immediate,delayed)
# bad_ids= uncleaned[items %in% c("lion tiger  cat dog  deer wolf", , prolific_id]
# 
# uncleaned[, .N, by= .(prolific_id, items, ageText, condition, gamenum)][max(N)]

