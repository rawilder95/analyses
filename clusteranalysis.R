rm(list= ls())
library(data.table)
library(ggplot2)
library(ez)
library(cowplot)

### Things to try ###
# -Subset of the methods (e.g. simdrop, dynamic etc.)
# - Using terminal python code, documentation in paper
# - 


# Define the which_condition function
setwd("~/Downloads/jatos_mac_java/analyses")
# Read in data; Current data file being used should be fluency_noerror.csv.  This is the spellechecked data file that excludes perseverations and participants that did not follow directions. 
dat= fread('fluency_noerror.csv')
# arrange a new data table that just preallocates prolific_id and items to textfile
tooutput_all= dat[, items, by= .(prolific_id, listnum, age, condition)]
# you need to upload each col identifier separately, since the software can only take subj id and words for columns
# use the file_idx variable to evaluate whether you need to write a new textfile
file_idx= list.files()
check_idx= c("clusteranalysis_delayoldlist1.txt", "clusteranalysis_immediateoldlist1.txt", "clusteranalysis_delayoldlist2.txt", "clusteranalysis_immediateoldlist2.txt")
# If there are not any instances where check_idx matches file_idx, run this next section of code
if(!any(file_idx %in% check_idx)){
}

#Immediate Condition
# immediate-young-list1
immediateyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Immediate",]
immediateyoungl1= subset(immediateyoungl1, select= c(prolific_id, items))
#Uncomment to write textfile
# write.table(immediateyoungl1, file = "clusteranalysis_immediateyounglist1.txt", sep = "\t", row.names = FALSE)
# immediate-young-list2
immediateyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Immediate",]
immediateyoungl2= subset(immediateyoungl2, select= c(prolific_id, items))
write.table(immediateyoungl2, file = "clusteranalysis_immediateyounglist2.txt", sep = "\t", row.names = FALSE)

# This section prepares a data table for a .txt file to be passed through the cluster analysis tool
# The tool returns a table with switch values (beginning of a cluster, continuation of a cluster, and cluster switches)
# immediate-old-list1
immediateoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Immediate",]
immediateoldl1= subset(immediateoldl1, select= c(prolific_id, items))
# write.table(immediateoldl1, file = "clusteranalysis_immediateoldlist1.txt", sep = "\t", row.names = FALSE)
# immediate-old-list2
immediateoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Immediate",]
immediateoldl2= subset(immediateoldl2, select= c(prolific_id, items))
# write.table(immediateoldl2, file = "clusteranalysis_immediateoldlist2.txt", sep = "\t", row.names = FALSE)
# Delayed Condition
# delayed-young-list1
delayedyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Immediate",]
delayedyoungl1= subset(delayedyoungl1, select= c(prolific_id, items))
# write.table(delayedyoungl1, file = "clusteranalysis_immediateyounglist1.txt", sep = "\t", row.names = FALSE)
# delayed-young-list2
delayedyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Immediate",]
delayedyoungl2= subset(delayedyoungl2, select= c(prolific_id, items))
# write.table(immediateyoungl2, file = "clusteranalysis_immediateyounglist2.txt", sep = "\t", row.names = FALSE)
# delayed-old-list1
delayedoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Immediate",]
delayedoldl1= subset(delayedoldl1, select= c(prolific_id, items))
# write.table(immediateoldl1, file = "clusteranalysis_delayoldlist1.txt", sep = "\t", row.names = FALSE)
# delayed-old-list2
delayedoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Immediate",]
delayedoldl2= subset(delayedoldl2, select= c(prolific_id, items))
# Uncomment if you need to use the SF cluster analysis tool
# write.table(immediateoldl2, file = "clusteranalysis_delayoldlist2.txt", sep = "\t", row.names = FALSE)
# Read in cluster csv's 
### Delayed Condition ###
# Delayed-Old-List 1
# Cluster switches
d1= fread("clusteranalysis_delayoldlist1_results/clusteranalysis_delayoldlist1_model_dynamic_switch_simdrop_switch_results.csv")
d1[, result:= "switch"]
# Model fit
nll= fread("clusteranalysis_delayoldlist1_results/clusteranalysis_delayoldlist1_model_dynamic_switch_simdrop_nll_results.csv"
)
# Set up col identifier for which part of the cluster results
nll[, result:= "model_fit"]
# Make data table to store all values in by factor 
delayold1= rbind(d1, nll, fill= TRUE)
# Model results
model_results= fread("clusteranalysis_delayoldlist1_results/clusteranalysis_delayoldlist1_model_dynamic_switch_simdrop_model_results.csv")
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
i1= fread("clusteranalysis_immediateoldlist1_results/clusteranalysis_immediateoldlist1_model_dynamic_switch_simdrop_switch_results.csv")
i1[, result:= "switch"]
nll= fread("clusteranalysis_immediateoldlist1_results/clusteranalysis_immediateoldlist1_model_dynamic_switch_simdrop_nll_results.csv")
model_results= fread("clusteranalysis_immediateoldlist1_results/clusteranalysis_immediateoldlist1_model_dynamic_switch_simdrop_model_results.csv")
immediateold1= rbind(i1, nll, model_results, fill= TRUE)
immediateold1[, listnum:= 1]
immediateold1[, age:= as.factor("Old")]
immediateold1[, condition:= as.factor("Immediate")]
immediateold1[, model:= "dynamic"]
immediateold1[, method:= "simdrop"]
### OLD LIST 1 ####
# Bind immediate and delayed old 1
oldl1= rbind(immediateold1, delayold1)

