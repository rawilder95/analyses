rm(list= ls())
library(data.table)
setwd('~/Downloads/jatos_mac_java/analyses')
spellfile= fread('snafuspellingdonottouch.csv')

# YOU IDIOT DO NOT TOUCH OR UNCOMMENT THIS FUCKING CODE UNLESS YOU WANT TO SPEND ANOTHER 2 HOURS DOING MANUAL SPELLING CORRECTIONS.  I KNOW YOU'RE GOING TO FORGET IF YOU LEAVE IT UNCOMMENTED BECAUSE YOU LITERALLY JUST DID THAT.
# spellfile= spellfile[spellfile[, order(correct),],]
# fwrite(spellfile, 'snafuspellingdonottouch.csv')
# schemefile= fread("updatedsnafuscheme.csv")
# addtoscheme= data.table(scheme= rep("TBA", length(spellfile[!correct %in% schemefile[, unique(word)], unique(correct)])), word= spellfile[!correct %in% schemefile[, unique(word)], unique(correct)])
# schemefile= rbind(schemefile, addtoscheme)
# filler_col= rep(NaN, length(icw))
# spellfile= rbind(spellfile, data.table(correct= filler_col, incorrect= icw))
# schemefile= schemefile[schemefile[, order(scheme),],]
# fwrite(schemefile, 'updatedsnafuscheme.csv')
# read in immediate
# immediate= fread('immediatefluency_merged.csv')
# # # immediate= subset(immediate, select= -c(starttime))
# delayed= fread('delayedfluency_merged.csv')
# delayed[, condition:= "Delayed"]
# fluency_data= rbind(immediate, delayed)
# fwrite(fluency_data,"fluency_data.csv")
## EXIT DANGERZONE ##
spellfile= fread('snafuspellingdonottouch.csv')
schemefile= fread('updatedsnafuscheme.csv')
fread("fluency_data.csv")
fluency_data[prolific_id== "5fc5b0ec967b5f13a297562a5fc5b0ec967b5f13a297562a", prolific_id:= "5fc5b0ec967b5f13a297562a"]
fluency_data[, items:= tolower(items)]
fluency_data[,items:=gsub(" ", "", items)]
fluency_data[,items:=gsub("-", "", items)]
qwords= fluency_data[!items %in% schemefile$word & !items %in% spellfile$incorrect, unique(items)]
bad_ids = fluency_data[!items %in% schemefile$word & !items %in% spellfile$incorrect, unique(prolific_id)]
fluency_data= fluency_data[!prolific_id %in% bad_ids]
fluency_data= fluency_data[shortlist== 0]
# incorrect words
# bad_ids= fluency_data[!items %in% schemefile$word & !items %in% spellfile$incorrect, prolific_id]
# immediate= immediate[!prolific_id %in% bad_ids,]
fluency_data[, age:= 0]
fluency_data[ageText< 60, age:= 0]
fluency_data[ageText>= 60, age:= 1]
fluency_data[, age:= as.character(age)]
fluency_data[age== 0, age:= "young"]
fluency_data[age== 1, age:= "old"]
fluency_data[, listnum:= as.character(gamenum)]

dat= fluency_data[, .N, by= .(prolific_id, age, condition, listnum)]

dat[, prolific_id:= as.factor(prolific_id)]
# dat[, listnum:= as.factor(listnum)]
dat[, age:= as.factor(age)]
dat[, condition:= as.factor(condition)]


ezANOVA(dat, wid = prolific_id, between = c("age", "condition"), within = listnum, dv = N, type= 3)

