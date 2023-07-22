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
#8dbdad66c9d72473e75f9e7fc068bb47529c8e76

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

psy205= data.table(openness= sample(100, replace= TRUE)/100, conscientiousness= sample(100, replace= TRUE)/100, extraversion= sample(100, replace= TRUE)/100, agreeableness= sample(100, replace= TRUE)/100, neuroticism= sample(100, replace= TRUE)/100)
psy205[, openness:= openness/0.9][openness>1, openness:= 1]
psy205[, extraversion:= extraversion/0.8][extraversion>1, extraversion:= 1]
psy205[, agreeableness:= agreeableness*0.6]
psy205[, neuroticism:= neuroticism*0.9]
psy205[, conscientiousness:= conscientiousness*0.4]
toplot= data.table(labels= c(rep("1Openness", 100), rep("2Conscientiousness", 100), rep("3Extraversion", 100), rep("4Agreeableness", 100), rep("5Neuroticism", 100)), scores= c(psy205$openness, psy205$conscientiousness, psy205$extraversion, psy205$agreeableness, psy205$neuroticism))
ggplot(data= psy205)+ geom_boxplot(aes(x= "1 Openness", y= openness))+ geom_boxplot(aes(x= "2  Extraversion", y= extraversion))+ geom_boxplot(aes(x= "3 Conscientiousness", y= conscientiousness))+ geom_boxplot(aes(x= "4 Agreeableness", y= agreeableness))+ geom_boxplot(aes(x= "5 Neuroticism", y= neuroticism))+ labs(x= "Personality Trait", y= "Standardized Score Estimation") + geom_point(aes(x= "1 Openness", y= openness), alpha= 0.1)+ geom_point(aes(x= "2  Extraversion", y= extraversion), alpha= 0.1)+ geom_point(aes(x= "3 Conscientiousness", y= conscientiousness), alpha= 0.1)+ geom_point(aes(x= "4 Agreeableness", y= agreeableness), alpha= 0.1)+ geom_point(aes(x= "5 Neuroticism", y= neuroticism), alpha= 0.1)


ggplot(data= witherrors) + geom_bar(aes(x= age, y= meanerror, fill= condition), position= 'dodge', stat= 'identity')+ labs(x= 'Age', y= 'Proportion of Errors', fill= "Delay")
d1= fread('cleanfluency.csv')
d1[gamenum==2, listnum:= 1]
d1[gamenum==1, listnum:= 2]
ggsave('celebrity_personality.png', device= 'png', dpi= 300)




# Semantic fluency analysis tool
sem_analysis= subset(dat, select= c(prolific_id, items))
colnames(sem_analysis) <- c('subject_id', 'words')
rownames(sem_analysis) <- NULL
write.table(sem_analysis, file = 'sem_analysis.txt', sep = '\t', row.names= FALSE )

# read-in online results 
cluster_switch= subset(fread('sem_analysis_model_none_switch_all_switch_results.csv'), select= -c(V1))

# get clusters >1
cluster_switch[Switch_Value>0]




# ggplot()+ geom_boxplot(aes(x= c("openness"),y= c(randu[1:4]$x)))

# immediate[, items:= tolower(items)]
# delayed[, items:= tolower(items)]
# uncleaned= rbind(immediate,delayed)
# bad_ids= uncleaned[items %in% c("lion tiger  cat dog  deer wolf", , prolific_id]
# 
# uncleaned[, .N, by= .(prolific_id, items, ageText, condition, gamenum)][max(N)]

