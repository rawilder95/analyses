options(scipen = 999)
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
p1= ggplot(data= toplot1, aes(x= age, y= V1))+ geom_bar(aes(fill= condition),stat= 'identity', position= 'dodge')+ theme_classic() + labs(x= "Age", y= "Response Length", fill= "Condition", title= "Mean Response Length: Trial 1")
toplot2= rl_anova[listnum==2, mean(N), by= .(condition, age)]
p2= ggplot(data= toplot2, aes(x= age, y= V1))+ geom_bar(aes(fill= condition),stat= 'identity', position= 'dodge')+ theme_classic() + labs(x= "Age", y= "Response Length", fill= "Condition", title= "Mean Response Length: Trial 2") + geom_text(label= "Significant Effects:", size=2, family= "Arial", aes(x= 1.5, y= 30))+ geom_text(label= "                   -Age", size=2, family= "Arial", aes(x= 1.5, y= 27))+ geom_text(label= "                   -Age by Delay Interaction", size=2, family= "Arial", aes(x= 1.5, y= 27))
cowplot::plot_grid(p1,p2)

