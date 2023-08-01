rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
source('bbar.r')
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ

# read in clean csv file
dat= fread('cleanfluency.csv')
# spellcheck
spellfile= fread('updatedsnafuspelling.csv')
schemefile= fread('updatedsnafuscheme.csv')
# check rows to see how # of errors
nrow(dat)
nrow(dat[items %in% schemefile$word,])
misspelled= dat[!items %in% schemefile$word]$items
spellfile[incorrect %in% misspelled,]
# swap the name gamenum for listnum
dat[, listnum:=gamenum]
dat= subset(dat, select= -c(gamenum))

# manual fix for timestamps
correct_timestamps= fread('fluencydata_cleaned062223.csv')
correct_timestamps[, times:= as.numeric(times)]
dat[, times:= as.numeric(times)]
merge(dat, correct_timestamps)




# get perseverations
# get id's
ids= dat[, .N, by= .(prolific_id, listnum, items)][N>1]$prolific_id
# get items
items= dat[, .N, by= .(prolific_id, listnum, items)][N>1]$items


