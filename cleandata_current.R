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
bad_id <- "951b39f1338dd2a61f0cf72a"
### IMMEDIATE ###
immediate_dt= fread('immediatefluency_merged.csv')
### DELAYED ###
# reset data table and other variables used
# This is an older version of the delayed code that works
delay_dt= fread('delayedfluency_merged.csv')
# fix the delay condition saying 'immediate'
delay_dt[, condition:= 'Delayed']
### COMBINED ###
dat= rbind(immediate_dt, delay_dt)
dat[, listnum:= gamenum]
dat= subset(dat, select = -c(gamenum))
# fix one error w prolific id
 dat[prolific_id== "645a93f0017bb2a61f0cf72a", prolific_id:= bad_id]
# swap listnum 2 & 1 for delayed condition (Jeff comment from prior meeting)
dat[condition== "Delayed" & listnum==2, listnum:= 3]
dat[condition== "Delayed" & listnum==1, listnum:= 2]
dat[condition== "Delayed" & listnum==3, listnum:= 1]
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
# dat[, items:=singularize(items)]
# Index errors that are in spellfile
# find participants that were not hitting return after each response
# 391 errors pre spellfile
# replace misspelled items with correct spelling
# dat[items %in% misspelled, items:= spellfile[sort(incorrect %in% misspelled), correct]]
j= 0
for(i in misspelled){
j= j+1
  if (nrow(dat[items %in% i])>0){
    dat[ items %in% i, items:= spellfile[incorrect %in% i, min(correct)]]
    if(any(is.na(dat[items %in% i, items]))){
      print(i)
    }
  } else{
    print('none')
  }
}
dat[, items:= tolower(items)]
# Get rid of whitespace
dat[,items:=gsub(" ", "", items)]
dat[items== "hummiiningbird", items:= "hummingbird"]
dat[items== "grow]undsqyirrel", items:= "squirrel"]
dat[items== "ringtailedlemur", items:= "lemur"]
stillerror= dat[!(items %in% schemefile$word), items]
# find participants that were not hitting return after each mov
bad_ids2 <- dat[items %in% stillerror[nchar(stillerror)>13], prolific_id]
bad_ids2<- c(bad_ids2, dat[items == "poop",unique(prolific_id)])
dat= dat[!prolific_id %in% bad_ids2]
# dat[ !(prolific_id %in% bad_ids2),]
# Write out code and then leave this commented out, unless you're looking to change something in base clean code.
# fwrite(dat, 'combinedfluency.csv')
dat= fread('combinedfluency.csv')
(stillerror= dat[!(items %in% schemefile$word) & !(items %in% spellfile$incorrect), items])
dat[!(prolific_id %in% dat[items %in% stillerror[nchar(stillerror)>13], unique(prolific_id)]),]
# trim the whitespace from the file
dat[,items:=gsub(" ", "", items)]
misspelled= dat[items %in% spellfile$incorrect, items]
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
