rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
# source('bbar.r')
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# read in clean csv file
### @Jeff would you mind seeing if this version of the csv is working on your end? I'm not sure if it's something going on with my version of R or a problem with the data.  Opening it, it looks fine. 
# dat= fread('fluency_data_07152023.csv')
dat= fread('fluencydata_cleaned062223.csv')




# spellcheck
spellfile= fread('updatedsnafuspelling.csv')
schemefile= fread('updatedsnafuscheme.csv')
# check rows to see how # of errors
nrow(dat)



nrow(dat[items %in% schemefile$word,])
misspelled= dat[!items %in% schemefile$word]$items
spellfile[incorrect %in% misspelled,]
# swap the name gamenum for listnum
# dat[, listnum:=gamenum]
# dat= subset(dat, select= -c(gamenum))
# # manual fix for timestamps
# correct_timestamps= fread('fluencydata_cleaned062223.csv')
# correct_timestamps[, times:= as.numeric(times)]
# dat[, times:= as.numeric(times)]
# merge(dat, correct_timestamps)
### UNCOMMENT TO FIX SPELLING ###
# for (i in misspelled){
#   if(nrow(dat[items %in% i]) > 0){
#     dat[items %in% i, items:= spellfile[incorrect == i, min(correct)]]
#     }
#   }
# get perseverations
# get id's
ids= dat[, .N, by= .(prolific_id, listnum, items)][N>1]$prolific_id
# get items
items= dat[, .N, by= .(prolific_id, listnum, items)][N>1]$items
# preallocate perseverations
dat[, perseveration:= 0]
# Index perseverations
p_idx= dat[, .N, by= .(prolific_id, listnum, items)][N>1]

dat= fread('fluency_data_07152023.csv')
for (i in 1:nrow(p_idx)){
  if(nrow(dat[prolific_id %in% p_idx[i]$prolific_id & items %in% p_idx[i]$items])>0){
   idx= dat[prolific_id %in% p_idx[i]$prolific_id & items %in% p_idx[i]$items]
   idx= idx[!itemnum== min(itemnum)]
   # exclude first instance
   dat[prolific_id %in% p_idx[i]$prolific_id & items %in% p_idx[i]$items & itemnum %in% idx$itemnum, perseveration:= 1]
  }
}

d1= fread('fluencydata_cleaned062223.csv')

d1[, .N, by= .(prolific_id, listnum, items)][N>1]

# add Age text
dat[, age:=  ageText>60]
# display perseverations
dat[, sum(perseverations)]
dat[, sum(perseverations), by= .(prolific_id, condition, age, listnum)]

dat[prolific_id %in% p_idx[i]$prolific_id & items %in% p_idx[i]$items]

