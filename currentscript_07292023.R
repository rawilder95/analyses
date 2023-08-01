rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
source('bbar.r')
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# Clean everything up and take out old code that doesn't do anything

# We know this data works
### Perseverations ###
dat= fread('fluency_data_07152023.csv')
perseverations= fread("perseverative_data.csv")
# mean perseverations = 0.013
toplot= perseverations[, mean(perseveration), by= .(age, listnum, condition)]
p1= ggplot(data= toplot[listnum== 1], (aes(x= age, y= V1, fill= condition)))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= "Perseverations List 1")+labs(title= "Perseverations List 1", y= "Proportion of Errors", x= "Age", fill= "Delay Condition")
p2= ggplot(data= toplot[listnum== 2], (aes(x= age, y= V1, fill= condition)))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= "Perseverations List 2", y= "Proportion of Errors", x= "Age", fill= "Delay Condition")
plot_grid(p1,p2)
ggsave('perseverations', device= 'png', dpi= 300)

anova_perseveration= perseverations[, mean(perseveration), by= .(prolific_id, age, condition, listnum)]

anova_perseveration[, prolific_id:= as.factor(prolific_id)]
anova_perseveration[, listnum:= as.factor(listnum)]
anova_perseveration[, condition:= as.factor(condition)]
anova_perseveration[, age:= as.factor(age)]
### @Jeff, this is where I am running into problems with ezANOVA it's saying that the df anova_perseveration is missing cells, but I've checked in several ways and its not
ezANOVA(data= anova_perseveration, within= listnum, between=c("age","condition"), dv= V1, wid=prolific_id)
# troubleshooting missing cells 
sum(is.na(anova_perseveration))

### Proportion Repeats ###
dat[is_repeat== "TRUE" & listnum== 1, is_repeat:= as.logical(0)]
dat[, is_repeat:= as.numeric(is_repeat)]
dat[is_repeat== "TRUE", proportion_repeat:= mean(is_repeat), by= .(prolific_id, listnum, condition, age)]
toplot= dat[listnum==2]
ggplot(data= toplot, aes(x= age, y= proportion_repeat, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(title= 'Proportion Repeat Trial 2', x= 'Age', y= 'Proportion Repeats', fill= 'Delay Condition')

anova_repeat= dat[listnum== 2, proportion_repeat:= mean(is_repeat), by= .(age,condition,prolific_id)]
anova_repeat= anova_repeat[listnum==2,]
anova_repeat[, listnum:= as.factor(listnum)]
anova_repeat[, age:= as.factor(age)]
anova_repeat[, prolific_id:= as.factor(prolific_id)]
ezANOVA(data= anova_repeat, between=c("age","condition"), dv=proportion_repeat, wid=prolific_id)

### Clustering analyses ###
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

# make new data table 344 rows 
cluster_switch[Switch_Value <2, sum(Switch_Value), by= .(prolific_id, listnum)]
# avg cluster size 
cluster_switch[Switch_Value ==0, switchvalnum:= seq, by= (prolific_id)]


# Merge clusters with dat
merge(dat, cluster_switch)

## All of the different cluster switch methods ##
# [1] "simdrop"                              "multimodal_alpha=0.0"                
# [3] "multimodal_alpha=0.1"                 "multimodal_alpha=0.2"                
# [5] "multimodal_alpha=0.30000000000000004" "multimodal_alpha=0.4"                
# [7] "multimodal_alpha=0.5"                 "multimodal_alpha=0.6000000000000001" 
# [9] "multimodal_alpha=0.7000000000000001"  "multimodal_alpha=0.8"                
# [11] "multimodal_alpha=0.9"                 "multimodal_alpha=1.0"                
# [13] "troyer"                               "delta_rise=0.0_fall=0.0"             
# [15] "delta_rise=0.0_fall=0.25"             "delta_rise=0.0_fall=0.5"             
# [17] "delta_rise=0.0_fall=0.75"             "delta_rise=0.0_fall=1.0"             
# [19] "delta_rise=0.25_fall=0.0"             "delta_rise=0.25_fall=0.25"           
# [21] "delta_rise=0.25_fall=0.5"             "delta_rise=0.25_fall=0.75"           
# [23] "delta_rise=0.25_fall=1.0"             "delta_rise=0.5_fall=0.0"             
# [25] "delta_rise=0.5_fall=0.25"             "delta_rise=0.5_fall=0.5"             
# [27] "delta_rise=0.5_fall=0.75"             "delta_rise=0.5_fall=1.0"             
# [29] "delta_rise=0.75_fall=0.0"             "delta_rise=0.75_fall=0.25"           
# [31] "delta_rise=0.75_fall=0.5"             "delta_rise=0.75_fall=0.75"           
# [33] "delta_rise=0.75_fall=1.0"             "delta_rise=1.0_fall=0.0"             
# [35] "delta_rise=1.0_fall=0.25"             "delta_rise=1.0_fall=0.5"             
# [37] "delta_rise=1.0_fall=0.75"             "delta_rise=1.0_fall=1.0"   


# toplot= dat[, mean(cluster_switch), by= ]
# ggplot(data= )+ geom

