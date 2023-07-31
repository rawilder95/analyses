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
#perseverations
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
ezANOVA(data= anova_perseveration, within= "listnum", between=c("prolific_id","age","condition"), dv= V1, wid=prolific_id)

# Repeats
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



