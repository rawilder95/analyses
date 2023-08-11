rm(list= ls())
library(data.table)
library(ggplot2)
library(ez)
library(cowplot)
# Define the which_condition function
setwd("~/Downloads/jatos_mac_java/analyses")
# Read in data; Current data file being used should be fluency_noerror.csv.  This is the spellechecked data file that excludes perseverations and participants that did not follow directions. 
dat= fread('fluency_noerror.csv')
### Response Length ###
# Preallocate data table for response length anova
# factor along id, condition, age, and trial number

rl_anova= dat[, .N, by= .(prolific_id, condition, age, listnum)]
# To avoid warnings, set these columns to be factors
rl_anova[, prolific_id:= as.factor(prolific_id)]
rl_anova[, condition:= as.factor(condition)]
rl_anova[, age:= as.factor(age)]
rl_anova[, listnum:= as.factor(listnum)]
rl_anova_results =ezANOVA(rl_anova, between=c("age","condition"), dv=N, within= listnum, wid=prolific_id)
# Error unbalanced
# How to check for the known error here (unbalanced with one or more cells missing data)
rl_anova[, length(unique(listnum)),by= .(prolific_id, condition, age)][!V1==2]
dat[prolific_id== "640cf44e8bf4e101d82a76a1"]
# get prolific_id s for those participants
# which prolific_ids are in list 1 but not list 2, vice versa
# In this instance Delayed old has ...
# 45 participants for list 1 
# 46 for list 2
# Delayed young has ...
# 39 for list 1
# 38 for list 2
# Source, participants inputting a typo for first trial of delay and different age for second demographics form. 
# Rule out anyone having different number of listnum 
# rl_anova[, length(unique(listnum)), by= .(prolific_id, condition, age)][!V1==2]$prolific_id
# rl_anova[prolific_id== "640cf44e8bf4e101d82a76a1"]
#one person manually fixed at the bottom of cleandata_current.R
# Rule out anyone having different conditions
rl_anova[, length(unique(condition)), by= .(prolific_id, condition, age)][!V1==1]$prolific_id
# not different number of conditions
# one person manually fixed at the bottom of cleandata_current.R
# how many observations per condition
# Plot results as a bargraph
# create a new data table for the plot
# Use these values to write the input for the labels manually
rl_anova_results$ANOVA$p[rl_anova_results$ANOVA$p<0.05]
rl_anova_results$ANOVA$F[rl_anova_results$ANOVA$p<0.05]
rl_anova_results$ANOVA$Effect[rl_anova_results$ANOVA$p<0.05]
toplot1= rl_anova[listnum==1, mean(N), by= .(condition, age)]
p1= ggplot(data= toplot1, aes(x= age, y= V1))+ geom_bar(aes(fill= condition),stat= 'identity', position= 'dodge')+ theme_classic() + labs(x= "Age", y= "Response Length", fill= "Condition", title= "Mean Response Length: Trial 1", subtitle= "Significant effects: Age ***, Age-Condition Interaction *")+ ylim(0,40)
toplot2= rl_anova[listnum==2, mean(N), by= .(condition, age)]
p2= ggplot(data= toplot2, aes(x= age, y= V1))+ geom_bar(aes(fill= condition),stat= 'identity', position= 'dodge')+ theme_classic() + labs(x= "Age", y= "Response Length", fill= "Condition", title= "Mean Response Length: Trial 2", subtitle= "F(1,176)= 41.66, p<0.001***, F(1,176)= 4.17786802, p= 0.042*")+ ylim(0,40)
cowplot::plot_grid(p1,p2)
ggsave('figures/responselengthbargraph.png', device= 'png', dpi= 300)
### Repetitions ###
# Use indexing method for perseverations but don't collapse across listnum (trial number).  Perseverations should all be masked out (i.e. check by nrow(dat[perseveration== 1])>0).
# Index counts for all items by participant.  Include listnum here to make it indexable
dat[, word_counts:=.N, by= .(prolific_id,items)]
dat[, repeated:=0]

# Only count trial 2 for repetitions
dat[word_counts>1 & listnum==2, repeated:=1]
# Input significant values from r_anova_results$ANOVA$[col identifier]
r_anova= dat[, mean(repeated), by= .(prolific_id, condition, age)]
# To avoid warnings, set these columns to be factors
r_anova[, prolific_id:= as.factor(prolific_id)]
r_anova[, condition:= as.factor(condition)]
r_anova[, age:= as.factor(age)]
r_anova_results =ezANOVA(r_anova, between=c("age","condition"), dv=V1, wid=prolific_id)
# plot bargraph
toplot= dat[listnum==2, mean(repeated), by= .(age, condition)]
ggplot(data= toplot, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(x= "Age", y= "Proportion Repeated Words", fill= "Condition", title= "Proportion of Responses Repeated", subtitle= "Trial 2 Only")+ theme_classic()
ggsave('figures/repetitionsbargraph.png', device= 'png', dpi= 300)


