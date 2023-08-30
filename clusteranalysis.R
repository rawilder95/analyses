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
setwd("cluster_analysis")
# Uncomment if you need to use the SF cluster analysis tool
# write.table(immediateoldl2, file = "delayoldlist2.txt", sep = "\t", row.names = FALSE)
# Read in cluster csv's 
### Cluster switches ###
# change the working directory to "cluster_analysis", where all of the relevant data files will be 
## Immediate Condition##
##Old Group##
# Immediate-Old-List 1
immediateoldl1=fread("immediateoldlist1_model_none_switch_simdrop_switch_results.csv")
immediateoldl1[, listnum:=1]
immediateoldl1[, itemnum:= 1:nrow(immediateoldl1)]
immediateoldl1[, age:= "Old"]
immediateoldl1[, condition:= "Immediate"]
# Immediate-Old-List 2
immediateoldl2=fread("immediateoldlist2_model_none_switch_simdrop_switch_results.csv")
immediateoldl2[, listnum:= 2]
immediateoldl2[, itemnum:= 1:nrow(immediateoldl2)]
immediateoldl2[, age:= "Old"]
immediateoldl2[, condition:= "Immediate"]
##Young Group##
# Immediate-Young-List 1
immediateyoungl1=fread("immediateyounglist1_model_none_switch_simdrop_switch_results.csv")
immediateyoungl1[, listnum:=1]
immediateyoungl1[, itemnum:= 1:nrow(immediateyoungl1)]
immediateyoungl1[, age:= "Young"]
immediateyoungl1[, condition:= "Immediate"]
# Immediate-Young-List 2
immediateyoungl2=fread("immediateyounglist2_model_none_switch_simdrop_switch_results.csv")
immediateyoungl2[, listnum:=2]
immediateyoungl2[, itemnum:= 1:nrow(immediateyoungl2)]
immediateyoungl2[, age:= "Young"]
immediateyoungl2[, condition:= "Immediate"]
## Delayed Condition##
##Old Group ##
# Delayed-Old-List 1
delayoldl1=fread("delayed_oldl1_model_none_switch_simdrop_switch_results.csv")
delayoldl1[, listnum:=1]
delayoldl1[, itemnum:= 1:nrow(delayoldl1)]
delayoldl1[, age:= "Old"]
delayoldl1[, condition:= "Delayed"]
# Delayed-Old-List 2
delayoldl2=fread("delayed_oldl2_model_none_switch_simdrop_switch_results.csv")
delayoldl2[, listnum:=2]
delayoldl2[, itemnum:= 1:nrow(delayoldl2)]
delayoldl2[, age:= "Old"]
delayoldl2[, condition:= "Delayed"]
##Young Group ##
# Delayed-Young-List 1
delayyoungl1=fread("delayedyounglist1_model_none_switch_simdrop_switch_results.csv")
delayyoungl1[, listnum:=1]
delayyoungl1[, itemnum:= 1:nrow(delayyoungl1)]
delayyoungl1[, age:= "Young"]
delayyoungl1[, condition:= "Delayed"]
# Delayed-Young-List 2
delayyoungl2=fread("delayed_youngl2_model_none_switch_simdrop_switch_results.csv")
delayyoungl2[, listnum:=2]
delayyoungl2[, itemnum:= 1:nrow(delayyoungl2)]
delayyoungl2[, age:= "Young"]
delayyoungl2[, condition:= "Delayed"]
# change directories back to the main analyses dir
setwd("~/Downloads/jatos_mac_java/analyses")
# merge with main data table



dat= fread("fluency_noerror.csv")
# preallocate cols for dat 
# R bind all of the data tables 
cluster_vals= rbind(delayoldl1, delayoldl2, delayyoungl1, delayyoungl2, immediateoldl1, immediateoldl2, immediateyoungl1, immediateyoungl2)
# remove V1 cols from cluster vals 
cluster_vals= subset(cluster_vals, select= -c(V1))
# Change name of col headers
cluster_vals[, prolific_id:= Subject]
cluster_vals[, items:= Fluency_Item]
# lower case other col values
cluster_vals[, switch_val:= Switch_Value]
cluster_vals[, switch_method:= Switch_Method]
# remove original col headers
cluster_vals= subset(cluster_vals, select= -c(Subject, Fluency_Item, Switch_Value, Switch_Method))
# Find number of cluster switches
# get the response length first (numerator for cluster sizes)
resp_len= cluster_vals[,  .N, by= .(prolific_id, listnum)]
# Weird work around but to prevent this from just copying over the original table and introduce the issues of NaNs and coerced integers (hopefully)
resp_len[, cluster_num:= N]
resp_len= subset(resp_len, select= -c(N))
# get the number of switches per trial
n_switches= cluster_vals[switch_val==1, .N, by= .(prolific_id, listnum)]
n_switches[, ns:= N]
n_switches= subset(n_switches, select= -c(N))
# get average cluster size
avg_cluster_size= merge(resp_len, n_switches)
avg_cluster_size[, avg_cluster:= cluster_num/ns]
# cluster_vals[, n_switches:= as.double(n_switches)]
# cluster_vals[, n_switches:= !(is.na(n_switches)), by= .(prolific_id, listnum)]
# cluster_vals[, avg_cluster_size:=resp_len/n_switches, by= .(prolific_id, listnum)]
# set data table for cluster size/switches ANOVA 
cluster_anova= merge(cluster_vals, avg_cluster_size, by= c("prolific_id", "listnum"))
cluster_size_anova= cluster_anova[, mean(avg_cluster), by= .(listnum, age, condition)]
cluster_switch_anova= cluster_anova[, mean(ns), by= .(listnum, age, condition)]
# Plot the rate of cluster switches
ggplot(data= cluster_switch_anova, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= "identity", position= "dodge")+ theme_classic()+ labs(title= "Rate of Cluster Switches", y= "Average # Switches", x= "Age", fill= "Condition")
ggsave("figures/avg_cluster_switch_bar.png", device= "png", dpi= 300)

# Plot the average cluster size
ggplot(data= cluster_size_anova, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= "identity", position= "dodge")







n_switches= cluster_vals[switch_val==1, .N, by= .(prolific_id, listnum, age, condition)]
# Find average cluster size
cluster_vals[, n_switches:= as.numeric(0)]
cluster_vals[, n_switches:= .N, by= .(prolific_id, listnum, age, condition)]
cluster_vals[, n_switches:= as.numeric(mean(n_switches)), by= .(listnum, age, condition)]
cluster_vals[, cluster_size:= as.numeric(.N), by = .(prolific_id, listnum, condition, age)]
# cluster_vals[switch_val==1, cluster_size:= mean(cluster_size/n_switches), by= .(prolific_id, age, condition, listnum)]
cluster_vals[, cluster_size:=as.numeric(cluster_size/n_switches), by= .(prolific_id, listnum, age, condition)]
cluster_vals[, cluster_size:= mean(cluster_size), by= .(listnum, age, condition)]






# So I've tried a lot of different things here, from setting the values to numeric to breaking the dt calculations into separate parts.  I'm lost as to why it's trying to coerce all values to being integer; 

# Back tracking on this for now... not sure if I'm going to approach it this way.
# Preset between vars as factors for ANOVA
# n_switches[, listnum:= as.factor(listnum)]
# n_switches[, prolific_id:= as.factor(prolific_id)]
# n_switches[, condition:= as.factor(condition)]
# n_switches[, age:= as.factor(age)]
# cluster_size[, listnum:= as.factor(listnum)]
# cluster_size[, prolific_id:= as.factor(prolific_id)]
# cluster_size[, condition:= as.factor(condition)]
# cluster_size[, age:= as.factor(age)]
# ANOVAs
ezANOVA(cluster_size, between=c("age","condition"), dv=V1, within= listnum, wid=prolific_id)
ezANOVA(n_switches, between=c("age","condition"), dv=N, within= listnum, wid=prolific_id)



# Error where merging messes up the order of observation and organization of rows from both DT's
# fix is to set order relative to listnum
this_subj[, .(prolific_id, items, age, condition, switch_val, switch_method), by= listnum]
# Check this subject 
# listnum out of order
# spotcheck: prolific_id= "5484655ffdf99b07b28f22cc"; age= "OLD", all 0's for switch value... which seems weird.
# order by listnum







# d1[, result:= "switch"]
# # Model fit
# nll= fread("delayoldlist1_results/delayoldlist1_model_dynamic_switch_simdrop_nll_results.csv"
# )
# # Set up col identifier for which part of the cluster results
# nll[, result:= "model_fit"]
# # Make data table to store all values in by factor 
# delayold1= rbind(d1, nll, fill= TRUE)
# # Model results
# model_results= fread("delayoldlist1_results/delayoldlist1_model_dynamic_switch_simdrop_model_results.csv")
# # Add to main delay old data table
# delayold1= rbind(delayold1, model_results, fill= TRUE)
# # Add listnum as a value because you're going to combine data tables by trial
# delayold1[, listnum:= 1]
# # Save yourself some time later and start adding back in the factors for the ANOVA
# delayold1[, age:= as.factor("Old")]
# delayold1[, condition:= as.factor("Immediate")]
# delayold1[, method:= "simdrop"]
# delayold1[, model:= "dynamic"]
# ## Repeat for each set of csv files 
# ### Immediate-Old-List 1 ###
# i1= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_switch_results.csv")
# i1[, result:= "switch"]
# nll= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_nll_results.csv")
# model_results= fread("immediateoldlist1_results/immediateoldlist1_model_dynamic_switch_simdrop_model_results.csv")
# immediateoldl1= rbind(i1, nll, model_results, fill= TRUE)
# immediateoldl1[, listnum:= 1]
# immediateoldl1[, age:= as.factor("Old")]
# immediateoldl1[, condition:= as.factor("Immediate")]
# immediateoldl1[, model:= "dynamic"]
# immediateoldl1[, method:= "simdrop"]
# ### OLD LIST 1 ####
# # Bind immediate and delayed old 1
# oldl1= rbind(immediateoldl1, delayold1)
# 
# Additional code but not sure if I need later
# ##Immediate Condition##
# ## Old Group ##
# # Immediate-Old-List 1
# immediateoldl1= fread("immediateoldlist1_model_none_switch_simdrop_switch_results.csv")
# immediateoldl1[, listnum:= 1]
# immediateoldl1[, itemnum:= 1:nrow(immediateoldl1)]
# # Immediate-Old-List 2
# immediateoldl2= fread("immediateoldlist2_model_none_switch_simdrop_switch_results.csv")
# immediateoldl2[, listnum:= 2]
# immediateoldl2[, itemnum:= 1:nrow(immediateoldl2)]
# ##Young Group
# # # Immediate-Young-List 1
# # immediateyoungl1=fread("immediateyounglist1_model_none_switch_simdrop_switch_results.csv")
# # immediateyoungl1[, listnum:= 1]
# # immediateyoungl1[, itemnum:= 1:nrow(immediateyoungl1)]
# # # Immediate-Young-List 2
# # immediateyoungl2=fread("immediateyounglist2_model_none_switch_simdrop_switch_results.csv")
# # immediateyoungl2[, listnum:= 2]
# # immediateyoungl2[, itemnum:= 1:nrow(immediateyoungl2)]
# 
# # for(i in dat[1, prolific_id]){
# #   # counter resets by subject
# #   counter= 0
# #   this_subj= dat[prolific_id==i,]
# #   # second for loop to go through each list
# #   for(j in 1:nrow(this_subj)){
# #     this_trial= this_subj[j]
# #     if(this_trial$switch_val==0){
# #       counter= counter+1
# #       }else{
# #         counter= counter+0
# #       }
# #     if(this_trial$switch_val>0){
# #     }
# }
# # # PAPERTRAIL @Jeff, this is the solution for the merge issue.  If both data.tables have unique columns merge will return an error and suggest allow.cartesian and the solution I put below.  You want by = .EACHi because that (cluster .csv data table) is the data table will fewer observations.
# # setkey(dat, prolific_id, listnum, itemnum, items)
# # setkey(cluster_vals, prolific_id, listnum, itemnum, items)
# # # We don't even necessarily need to merge
# # # k= merge(dat, cluster_vals, key_by= ("prolific_id"))
# # 
# # dat[,switch_val]
# # # create a temp data table that parses the switch value by prolific_id and listnum
# # temp_dt= dat[, switch_val, by= .(prolific_id, listnum)]
# # For loop through each participant (start with 1 for now)