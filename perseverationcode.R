rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
# source('bbar.r')
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# Read in clean working data
dat= fread('temp_files/combinedfluency.csv')
### Uncomment to double-check that file has no spelling errors ###
# schemefile= fread('updatedsnafuscheme.csv')
# dat[!items %in% schemefile$word, items]
# get counts for all of the words that were listed for each trial
dat[, word_counts:=.N, by= .(prolific_id, condition, listnum, items)]
# make a perseverations col
dat[, perseveration:= 0]
# Here we index *all* instances where an item was listed more than once per trial.  On the next line we exclude cases where it was the first time an item was listed
dat[word_counts>1, perseveration:=1]
# exclude the first time the item was generated (smallest itemnum) because this isn't a perseveration
dat[perseveration== 1 & itemnum== min(itemnum), perseveration:=0, by= .(prolific_id,items)]
#Bar graph for perseveration rate
toplot1= dat[listnum==1, mean(perseveration), by= .(listnum,condition, age)]
toplot2= dat[listnum==2, mean(perseveration), by= .(listnum,condition, age)]
p1= ggplot(data= toplot1, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(x= 'Age', y= 'Perseveration Rate', fill= "Condition", title= "Perseverative Errors Trial 1")+ theme_classic()+ylim(0,0.1)
p2= ggplot(data= toplot2, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(x= 'Age', y= 'Perseveration Rate', fill= "Condition", title= "Perseverative Errors Trial 2")+theme_classic()+ylim(0,0.1)
plot_grid(p1,p2)
ggsave('figures/perseverationsbargraph.png', device= 'png', dpi= 300)

# Write data file with perseverations
# create a variable for perseverations to keep everything compartmentalized
# which items were said more than once on any particular trial?
# *perseveration* = col identifier in dat data.table
# Create data table without perseverations to write out for subsequent analyses. Itemnum preserves ordinal position
# Check to see if Delayed listnum 2 times are actually from listnum 1
# If that's the case than it should be negative
fluency_noerror= dat[perseveration==0]
# stays here because this is the main file
fwrite(fluency_noerror, 'fluency_noerror.csv')
# Check that the issue with rows shifting hasn't happened again
# print(dat[, unique(condition), prolific_id])
# ezANOVA code here
# stop here
# Back up in case perseveration data gets messed up 
# withperseverations= fread('with_perseverations.csv')
# dat= withperseverations
# rewrite file for perseveration data.table
# fwrite(dat, 'combinedfluency.csv')
# Everything with this problematic subject dat[prolific_id== "640cf44e8bf4e101d82a76a1"] looks fine here
# For some reason at some point some line of code causes the age identifiers to say "Young" because they had the typo of writing 52 for age
dat= fread('temp_files/with_perseverations.csv')
k= fread('fluency_noerror.csv')
k[,length(unique(prolific_id)), by= .(age)]

j= k[, unique(prolific_id), by= .(condition, age)][, .N, by= .(condition, age)]


ggplot(data= j, aes(x= age, y= N, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ theme_classic()+ labs(x= "Age", y= "N Participants", fill= "Condition")
ggsave('figures/participants_per_group.png', device= 'png', dpi= 300)


        # Input significant values from perseveration_anova_results$ANOVA$[col identifier]
        perseveration_anova= dat[, mean(perseveration), by= .(prolific_id, condition, age, listnum)]
        
        # To avoid warnings, set these columns to be factors
        perseveration_anova[, prolific_id:= as.factor(prolific_id)]
        perseveration_anova[, condition:= as.factor(condition)]
        perseveration_anova[, age:= as.factor(age)]
        perseveration_anova[, listnum:= as.factor(listnum)]
        
        
        
perseveration_anova_results =ezANOVA(perseveration_anova, between=c("age","condition"), dv=V1, wid=prolific_id, within= listnum)$ANOVA















































