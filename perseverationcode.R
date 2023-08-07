rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
# source('bbar.r')
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# Read in clean working data
dat= fread('combinedfluency.csv')
### Uncomment to double-check that file has no spelling errors ###
# schemefile= fread('updatedsnafuscheme.csv')
# dat[!items %in% schemefile$word, items]
# get counts for all of the words that were listed for each trial
dat[, word_counts:=.N, by= .(prolific_id, condition, listnum, items)]
# make a perseverations col
dat[, perseveration:= 0]
dat[word_counts>1, perseveration:=1]
# do bar graph
toplot1= dat[listnum==1, mean(perseveration), by= .(listnum,condition, age)]
toplot2= dat[listnum==2, mean(perseveration), by= .(listnum,condition, age)]
p1= ggplot(data= toplot1, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(x= 'Age', y= 'Perseveration Rate', fill= "Condition", title= "Perseverative Errors Trial 1")+ theme_classic()
p2= ggplot(data= toplot2, aes(x= age, y= V1, fill= condition))+ geom_bar(stat= 'identity', position= 'dodge')+ labs(x= 'Age', y= 'Perseveration Rate', fill= "Condition", title= "Perseverative Errors Trial 2")+theme_classic()
cowplot::plot_grid(p1,p2)

# which items were said more than once on any particular trial
# *perseverations* = data.table
# *perseveration* = col identifier in dat data.table
perseverations= dat
# Now get rid of perseverations in dat (itemnum will mark still mark the ordinal positions)
dat[perseveration== 0]
# We set all words that were repeated more than once to 1 *initially* (before going back and setting the smallest itemnum to 0)
perseverations[perseveration== 1]
# reset the smallest itemnum (first instance) to 0 because it is not a perseveration
perseverations[perseveration== 1 & itemnum== min(itemnum), perseveration:=0, by= .(prolific_id,items)]
# Repeat the perseveration process in main data table so that you can later exclude the values
dat[, perseveration:= 0]
# Match the items in the persev and dat by participant and listnum (word i was said x times by participant z on trial y)
dat[, .N, by= .(prolific_id, listnum, items)][N>1]
# Check that the issue with rows shifting hasn't happened again
  # print(dat[, unique(condition), prolific_id])

# ezANOVA code here
# stop here
  
































































