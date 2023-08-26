rm(list= ls())
library(data.table)
library(ggplot2)
library(ez)
library(cowplot)
### Things to try ###
# -Subset of the methods (e.g. simdrop, dynamic etc.)
# - Using terminal python code, documentation in paper
# For cluster switches 
# How many clusters in this list
# Average size of the clusters
# Start with one of those methods like sim_drop
# fread('clusterswitches.csv')
# All you have to do is change the DV
# Repetition cluster size: How long are the repetition chains, are they more interleaved or all together (e.g. new new new old old old)
# Define the which_condition function
setwd("~/Downloads/jatos_mac_java/analyses")
# Read in data; Current data file being used should be fluency_noerror.csv.  This is the spellechecked data file that excludes perseverations and participants that did not follow directions. 
dat= fread('fluency_noerror.csv')
# arrange a new data table that just preallocates prolific_id and items to textfile
tooutput_all= dat[, items, by= .(prolific_id, listnum, age, condition)]
# you need to upload each col identifier separately, since the software can only take subj id and words for columns
# List all of the file names in the folder called "cluster_analysis"
file_idx= list.files("cluster_analysis")
# preallocate file names to check whether creating .txt files is necessary.   
check_idx= c("delayoldlist1.txt", "immediateoldlist1.txt", "delayoldlist2.txt", "immediateoldlist2.txt")
### Function Name Format: condition_age_l# -> [delayed or immediate]_[old or young]_[list 1 or 2]
# immediate-young-list1
immediate_young_l1= function(r){
  #Immediate Condition
  # immediate-young-list1
  immediateyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Immediate",]
  immediateyoungl1= subset(immediateyoungl1, select= c(prolific_id, items))
  #write out to cluster_analysis
  write.table(immediateyoungl1, file = "cluster_analysis/immediateyounglist1.txt", sep = "\t", row.names = FALSE)
  print('run')
}
# immediate-young-list2
immediate_young_l2= function(r){
  if (r==1){
immediateyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Immediate",]
immediateyoungl2= subset(immediateyoungl2, select= c(prolific_id, items))
write.table(immediateyoungl2, file = "immediateyounglist2.txt", sep = "\t", row.names = FALSE)}
}
# This section prepares a data table for a .txt file to be passed through the cluster analysis tool
# The tool returns a table with switch values (beginning of a cluster, continuation of a cluster, and cluster switches)
# immediate-old-list1
immediate_old_l1= function(r){
immediateoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Immediate",]
immediateoldl1= subset(immediateoldl1, select= c(prolific_id, items))
write.table(immediateoldl1, file = "cluster_analysis/immediateoldlist1.txt", sep = "\t", row.names = FALSE)
}
# immediate-old-list2
immediate_old_l2= function(r){
  if (r==1){
    immediateoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Immediate",]
    immediateoldl2= subset(immediateoldl2, select= c(prolific_id, items))
    write.table(immediateoldl2, file = "immediateoldlist2.txt", sep = "\t", row.names = FALSE)
    }
}
# delay-young-l2
delayed_young_l1= function(r){

    # Delayed Condition
    # delayed-young-list1
    delayedyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Delayed",]
    delayedyoungl1= subset(delayedyoungl1, select= c(prolific_id, items))
    write.table(delayedyoungl1, file = "cluster_analysis/delayedyounglist1.txt", sep = "\t", row.names = FALSE)
}
delayed_young_l1(1)
# delay-young-l2
delayed_young_l2= function(r){
    # Delayed Condition
    # delayed-young-list2
    delayedyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Delayed",]
    delayedyoungl2= subset(delayedyoungl2, select= c(prolific_id, items))
    write.table(delayedyoungl2, file = "cluster_analysis/delayed_youngl2.txt", sep = "\t", row.names = FALSE)
}
# delayed-old-list1
delayed_old_l1= function(r){
    delayedoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Delayed",]
    delayedoldl1= subset(delayedoldl1, select= c(prolific_id, items))
    write.table(delayedoldl1, file = "cluster_analysis/delayed_oldl1.txt", sep = "\t", row.names = FALSE)
}
# delayed-old-list2
delayed_old_l2= function(r){
    delayedoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Delayed",]
    delayedoldl2= subset(delayedoldl2, select= c(prolific_id, items))
    write.table(delayedoldl2, file = "cluster_analysis/delayed_oldl2.txt", sep = "\t", row.names = FALSE)
}
# Look for any instances where check_idx matches file_idx
# If the files do not exist yet, create them and write them into the folder cluster_analysis
# Run all the functions to create a .txt file for each condition. 
# If the files do not exist
immediate_young_l1(1)
immediate_young_l2(1)
immediate_old_l1(1)
immediate_old_l2(1)
delayed_young_l1(1)
delayed_young_l2(1)
delayed_old_l1(1)
delayed_old_l2(1)








# Uncomment if you need to use the SF cluster analysis tool
# write.table(immediateoldl2, file = "delayoldlist2.txt", sep = "\t", row.names = FALSE)
# Read in cluster csv's 
### Cluster switches ###
# change the working directory to "cluster_analysis", where all of the relevant data files will be 
setwd("cluster_analysis")
## Delayed Condition##
# Delayed-Old-List 1

d1= fread("delayoldlist1_results/delayoldlist1_model_dynamic_switch_simdrop_switch_results.csv")


d1[, result:= "switch"]
# Model fit
nll= fread("delayoldlist1_results/delayoldlist1_model_dynamic_switch_simdrop_nll_results.csv"
)
# Set up col identifier for which part of the cluster results
nll[, result:= "model_fit"]
# Make data table to store all values in by factor 
delayold1= rbind(d1, nll, fill= TRUE)
# Model results
model_results= fread("delayoldlist1_results/delayoldlist1_model_dynamic_switch_simdrop_model_results.csv")
# Add to main delay old data table
delayold1= rbind(delayold1, model_results, fill= TRUE)
# Add listnum as a value because you're going to combine data tables by trial
delayold1[, listnum:= 1]
# Save yourself some time later and start adding back in the factors for the ANOVA
delayold1[, age:= as.factor("Old")]
delayold1[, condition:= as.factor("Immediate")]
delayold1[, method:= "simdrop"]
delayold1[, model:= "dynamic"]
## Repeat for each set of csv files 
### Immediate-Old-List 1 ###
i1= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_switch_results.csv")
i1[, result:= "switch"]
nll= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_nll_results.csv")
model_results= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_model_results.csv")
immediateoldl1= rbind(i1, nll, model_results, fill= TRUE)
immediateoldl1[, listnum:= 1]
immediateoldl1[, age:= as.factor("Old")]
immediateoldl1[, condition:= as.factor("Immediate")]
immediateoldl1[, model:= "dynamic"]
immediateoldl1[, method:= "simdrop"]
### OLD LIST 1 ####
# Bind immediate and delayed old 1
oldl1= rbind(immediateoldl1, delayold1)

