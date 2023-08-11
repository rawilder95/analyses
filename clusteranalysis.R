rm(list= ls())
library(data.table)
library(ggplot2)
library(ez)
library(cowplot)
# Define the which_condition function
setwd("~/Downloads/jatos_mac_java/analyses")
# Read in data; Current data file being used should be fluency_noerror.csv.  This is the spellechecked data file that excludes perseverations and participants that did not follow directions. 
dat= fread('fluency_noerror.csv')
# arrange a new data table that just preallocates prolific_id and items to textfile
tooutput_all= dat[, items, by= .(prolific_id, listnum, age, condition)]
# you need to upload each col identifier separately, since the software can only take subj id and words for columns
#Immediate Condition
# immediate-young-list1
immediateyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Immediate",]
immediateyoungl1= subset(immediateyoungl1, select= c(prolific_id, items))
write.table(immediateyoungl1, file = "clusteranalysis_immediateyounglist1.txt", sep = "\t", row.names = FALSE)
# immediate-young-list2
immediateyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Immediate",]
immediateyoungl2= subset(immediateyoungl2, select= c(prolific_id, items))
write.table(immediateyoungl2, file = "clusteranalysis_immediateyounglist2.txt", sep = "\t", row.names = FALSE)
# immediate-old-list1
immediateoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Immediate",]
immediateoldl1= subset(immediateoldl1, select= c(prolific_id, items))
write.table(immediateoldl1, file = "clusteranalysis_immediateoldlist1.txt", sep = "\t", row.names = FALSE)
# immediate-old-list2
immediateoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Immediate",]
immediateoldl2= subset(immediateoldl2, select= c(prolific_id, items))
write.table(immediateoldl2, file = "clusteranalysis_immediateoldlist2.txt", sep = "\t", row.names = FALSE)
# Delayed Condition
# delayed-young-list1
delayedyoungl1= tooutput_all[age== "Young" & listnum==1 & condition== "Immediate",]
delayedyoungl1= subset(delayedyoungl1, select= c(prolific_id, items))
write.table(delayedyoungl1, file = "clusteranalysis_immediateyounglist1.txt", sep = "\t", row.names = FALSE)
# delayed-young-list2
delayedyoungl2= tooutput_all[age== "Young" & listnum==2 & condition== "Immediate",]
delayedyoungl2= subset(delayedyoungl2, select= c(prolific_id, items))
write.table(immediateyoungl2, file = "clusteranalysis_immediateyounglist2.txt", sep = "\t", row.names = FALSE)
# delayed-old-list1
delayedoldl1= tooutput_all[age== "Old" & listnum==1 & condition== "Immediate",]
delayedoldl1= subset(delayedoldl1, select= c(prolific_id, items))
write.table(immediateoldl1, file = "clusteranalysis_delayoldlist1.txt", sep = "\t", row.names = FALSE)
# delayed-old-list2
delayedoldl2= tooutput_all[age== "Old" & listnum==2 & condition== "Immediate",]
delayedoldl2= subset(delayedoldl2, select= c(prolific_id, items))
write.table(immediateoldl2, file = "clusteranalysis_delayoldlist2.txt", sep = "\t", row.names = FALSE)


