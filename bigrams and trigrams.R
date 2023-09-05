library(cowplot)
# Define the which_condition function
setwd("~/Downloads/jatos_mac_java/analyses")
# Read in data; Current data file being used should be fluency_noerror.csv.  This is the spellechecked data file that excludes perseverations and participants that did not follow directions. 
dat= fread('fluency_noerror.csv')


# sample subject/trial
this_subj= dat[prolific_id== prolific_id[1]]
this_subj[repeated== 1]


