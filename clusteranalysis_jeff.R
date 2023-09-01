library(data.table)
library(ez)
# Load fluency data without perseverations
dat <- fread("fluency_noerror.csv")
# @Rebecca The line below shows that there are two participants with
# perseverations in the data.  This will cause problems, so I will temporarily
# remove these participants, but please investigate.
bad_ids= dat[,.N,by=.(prolific_id,listnum,items)][N==2]
dat[prolific_id %in% bad_ids$prolific_id]
# Remove participants with perseverations for now
bad_ids <- dat[,.N,by=.(prolific_id,listnum,items)][N==2, prolific_id]
dat <- dat[!(prolific_id %in% bad_ids)]
# Load cluster switch output from forager
f1 <- fread("cluster_analysis/delayed_oldl1_model_none_switch_simdrop_switch_results.csv")
f2 <- fread("cluster_analysis/immediateoldlist1_model_none_switch_simdrop_switch_results.csv")
f3 <- fread("cluster_analysis/delayedyounglist1_model_none_switch_simdrop_switch_results.csv")
f4 <- fread("cluster_analysis/immediateyounglist1_model_none_switch_simdrop_switch_results.csv")
f5 <- fread("cluster_analysis/delayed_oldl2_model_none_switch_simdrop_switch_results.csv")
f6 <- fread("cluster_analysis/immediateoldlist2_model_none_switch_simdrop_switch_results.csv")
f7 <- fread("cluster_analysis/delayed_youngl2_model_none_switch_simdrop_switch_results.csv")
f8 <- fread("cluster_analysis/immediateyounglist2_model_none_switch_simdrop_switch_results.csv")
# Combine files into "List 1" and "List 2"
list1 <- rbind(f1, f2, f3, f4)
list2 <- rbind(f5, f6, f7, f8)
# Add listnum
list1[,listnum := 1]
list2[,listnum := 2]
# Align column names to dat
setnames(list1, "Subject", "prolific_id")
setnames(list1, "Fluency_Item", "items")
setnames(list2, "Subject", "prolific_id")
setnames(list2, "Fluency_Item", "items")
# Combine switch data into one data table and drop unneeded columns
switch_data <- rbind(list1, list2)
switch_data[, V1 := NULL]
switch_data[, Switch_Method := NULL]
# @Rebecca This data table has 7294 rows but "dat" has 7695 rows. This means
# that some data or some lists do not have switch values. Please investigate.
# @Rebecca The line below shows that there three participants with
# perseverations in the forager output (which will cause problems). Two of them
# overlap with those found in "dat" (line 10), but there is also a third
# participant. Please investigate.
switch_data[,.N, by=.(prolific_id, listnum, items)][N==2]
bad_id3= dat[prolific_id== "63d53c6bc368b9fa8d33f4cf"]
bad_id3[, order(listnum), by= prolific_id]
# Remove participants with perseverations for now
bad_ids <- switch_data[,.N,by=.(prolific_id,listnum,items)][N==2, prolific_id]
switch_data <- switch_data[!(prolific_id %in% bad_ids)]
bad_id3= dat[prolific_id== "63d53c6bc368b9fa8d33f4cf"]
# Somewhere this participants listnum 2 was overwritten to say listnum 1.  But this is fine after perseverations and throughout repetition_responselength
# manually found that dog-cat-elephant-fox-raccoon are the first 5 responses (look for that sequence)
bad_id3
# merge fluency data with switch data
setkey(dat, prolific_id, listnum, items)
setkey(switch_data, prolific_id, listnum, items)
dat <- merge(dat, switch_data, all.x=TRUE)
# Look at the data that you are inputting to forager 

dat[is.na(Switch_Value), .N, by= prolific_id]


# @Rebecca The line below shows there are lots of responses in the data that do
# not have switch values. Most of them are common animals that should be in the
# forager database. Please investigate. 
dat[is.na(Switch_Value)]
# Add a column for total number of responses per list
dat[, number_of_responses := .N, by=.(prolific_id, listnum)]
# For analysis, we want only one row per list
anova_dat <- unique(dat[,.(prolific_id, age, condition, listnum, number_of_responses)])
# Add a column for number of cluster switches per list
tmp <- dat[Switch_Value==1, .(number_of_cluster_switches = .N), by=.(prolific_id, listnum, age, condition)]
# @Rebecca this data table has 6 fewer rows, meaning there are 6 lists that have no switches (Switch_Value == 1)
# Closer inspection shows that these participants have lots of NAs for switch values, please investigate.
anova_dat <- merge(anova_dat, tmp)

# Add a column for average cluster size per list
anova_dat[, avg_cluster_size := number_of_responses / number_of_cluster_switches]

# @Rebecca The line below shows that several participants don't have both lists
# (probably resulting from the issues above).  Please investigate. I will
# temporarily remove.
anova_dat[,.N,by=prolific_id][,N]

# Remove participants with only one list, temporarily.
bad_ids <- anova_dat[,.N,by=prolific_id][N==1,prolific_id]
anova_dat <- anova_dat[!(prolific_id %in% bad_ids)]

# Change listnum to factor for analysis
anova_dat[,listnum := factor(listnum)]

# Q1: Are there differences in number of cluster switches per condition?
anova_dat[, mean(number_of_cluster_switches), by=.(age, condition, listnum)]

# only age is significant
ezANOVA(dat=anova_dat,
        wid=prolific_id,
        within=listnum, 
        between=c("age","condition"),
        dv=number_of_cluster_switches)

# A1: Younger adults have more cluster switches, but that's all. (Number of
# cluster switches is highly correlated with number of responses, so not
# terribly surprising).
anova_dat[, mean(number_of_cluster_switches), by=.(age)]

# Q2: Are there differences in average cluster size per condition?
anova_dat[, mean(avg_cluster_size), by=.(age, condition, listnum)]

# no significant differences
ezANOVA(dat=anova_dat,
        wid=prolific_id,
        within=listnum, 
        between=c("age","condition"),
        dv=avg_cluster_size)
