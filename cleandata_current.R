rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
library(rjson)
library(SemNetCleaner)
library(brio)
# source('bbar.r')
setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
# get id for copied over prolific_id
# bad_id <- "951b39f1338dd2a61f0cf72a"
### IMMEDIATE ###
immediate_dt= fread('immediatefluency_merged.csv')
### DELAYED ###
# reset data table and other variables used
# This is an older version of the delayed code that works
delay_dt= fread('delayedfluency_merged.csv')
# To address issue where delay_dt has 'immediate col identifier'.
# delay_dt= subset(delay_dt, select= -c(condition))
# delay_dt[, condition:= "Delayed"]
rbind(immediate_dt, delay_dt)
dat= rbind(immediate_dt, delay_dt)
# Double check to make sure that nobody has two different conditions
dat[, unique(condition), by= .(prolific_id)]
### COMBINED ###
dat[, listnum:= gamenum]
dat= subset(dat, select = -c(gamenum))
  #Not breaking up to here
# fix one error w prolific id
# dat[prolific_id== "645a93f0017bb2a61f0cf72a", prolific_id:= bad_id]
# get bad id's for participants who did both conditions
# swap listnum 2 & 1 for delayed condition (Jeff comment from prior meeting)
dat[condition== "Delayed" & listnum==2, listnum:= 3]
dat[condition== "Delayed" & listnum==1, listnum:= 2]
dat[condition== "Delayed" & listnum==3, listnum:= 1]
dat[, unique(condition), by= .(prolific_id)]


###IMPORTANT: THIS LINE OF CODE CHECKS FOR THE MERGE ROW SHIFT ISSUE ###
#Rows can sometimes be duplicated for certain col identifiers with various dt functions
# dat[, unique(condition), by= prolific_id]

# fluency_data[prolific_id, (times-starttime)/1000]
### READ IN SNAFU ###
schemefile= fread('updatedsnafuscheme.csv')
spellfile= fread('updatedsnafuspelling.csv')
# find all items not in scheme
# length(dat[items %in% schemefile$word, items]) #758
# nrow(dat) #8905
# set all words to be lowercase
# combine and write out table. Comment out, but now you have something reproducible.
# get rid of plural words 

# Uncomment to singularize- but this takes a long time
# for(i in 1:nrow(dat)){
#   dat[i, items:= singularize(items)]
# }
# dat[, unique(condition), by= .(prolific_id)]

# Index errors that are in spellfile
# find participants that were not hitting return after each response
# 391 errors pre spellfile
# replace misspelled items with correct spelling

# this might have been the error.  There was a line that overwrote dat.
# dat= fread('combinedfluency.csv')
# (stillerror= dat[!(items %in% schemefile$word) & !(items %in% spellfile$incorrect), items])
dat[, items:= tolower(items)]
# Get rid of whitespace
dat[,items:=gsub(" ", "", items)]
dat[items== "hummiiningbird", items:= "hummingbird"]
dat[items== "grow]undsqyirrel", items:= "squirrel"]
dat[items== "ringtailedlemur", items:= "lemur"]
stillerror= dat[!(items %in% schemefile$word), items]
dat[!(prolific_id %in% dat[items %in% stillerror[nchar(stillerror)>13], unique(prolific_id)]),]
# trim the whitespace from the file
dat[,items:=gsub(" ", "", items)]
misspelled= dat[items %in% spellfile$incorrect, items]
j= 0
for(i in misspelled){
  j= j+1
  if (nrow(dat[items %in% i])>0){
    dat[ items %in% i, items:= spellfile[incorrect %in% i, min(correct)]]
}
# exclude participants that listed musical instruments or clearly did not follow directions
# get id
if(nrow(dat[items=="trombone"])>0){
  dat=dat[!prolific_id== dat[items== "trombone", prolific_id],]
  }
}
# find participants that were not hitting return after each mov
bad_ids2 <- dat[items %in% stillerror[nchar(stillerror)>13], prolific_id]
bad_ids2<- c(bad_ids2, dat[items == "poop",unique(prolific_id)])
dat= dat[!prolific_id %in% bad_ids2]
# dat[!(prolific_id %in% bad_ids2),]
# Write out code and then leave this commented out, unless you're looking to change something in base clean code.
# fwrite(dat, 'combinedfluency.csv')
# Get rid of all of the known spelling errors
for(i in misspelled){
 dat[items == i, items:= spellfile[incorrect == i, min(correct)]]
}
dat[items %in% spellfile$incorrect,items]
# There should not be any known errors after this
# Unknown errors
dat[!items %in% schemefile$word,items]
# Should show two NaNs and nothing else
# From here on use combined fluency, this is the cleaned code that excludes spelling errors and corrects pluralities, whitespace, but not perseverative errors. 
#add age 
dat[, age:= "Young"]
dat[ageText>= 60, age:= "Old"]
dat[, mean(ageText), by= age]
# I forgot to add back in the age text earlier, so I'm writing it in here.  It's now in the datafile so this line can stay uncommented.  I'm trying to rewrite the file as few times as possible to avoid some of the issues we've been seeing with the datafile.
# get all delayed condition participants that only did one trial
j= dat[condition== "Delayed" & listnum== 1, unique(prolific_id)]
k= dat[condition== "Delayed" & listnum== 2, unique(prolific_id)]
# Get rid of that participant
dat= dat[!prolific_id %in% j[!j %in% k],]
dat[!prolific_id== "5c17a9fbfeaf2c0001c4b19a"]
fwrite(dat, 'combinedfluency.csv')
# person with NaN input mask out (Jeff instructions from lab meeting)
# dat[, unique(condition), by= .(prolific_id)]

