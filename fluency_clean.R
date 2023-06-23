rm(list= ls())
library(data.table)
# library(ggplot2)
# Define the which_condition function
setwd("/Users/rebeccawilder/Downloads/jatos_mac_java/analyses")
list.files(pattern= '*snafu')
immediate= fread("immediate_fluency.csv")
delayed= fread("delayed_fluency.csv")
spellfile= fread("animals_snafu_spellfile.csv")
schemefile= fread("animals_snafu_scheme.csv")
colnames(spellfile) = c("correct", "incorrect")
colnames(schemefile)= c("scheme", "word")
fluency_data= rbind(immediate,delayed)
# lowercase and remove whitespace
fluency_data[, items:= tolower(items)]
fluency_data[,items:=gsub(" ", "", items)]
# fix id errors 
fluency_data[prolific_id== "5f402eb4fbd8df145068b8055f402eb4fbd8df145068b805", prolific_id:= "5f402eb4fbd8df145068b805"]
fluency_data[prolific_id== "622a2074fe80192aa7a0212a622a2074fe80192aa7a0212a", prolific_id:= "622a2074fe80192aa7a0212a"]
# SCREEN THIS PARTICIPANT OUT 
bad_ids <- c(fluency_data[prolific_id== "5c6ae4c31b9d0e000190bacc", unique(prolific_id)], fluency_data[items== "poop", unique(prolific_id)], fluency_data[items== "piano", unique(prolific_id)])
# find long strings
long_string= fluency_data[, nchar(items)>16, items][V1=="TRUE", items]
bad_ids <- c(bad_ids, fluency_data[items %in% long_string, prolific_id],fluency_data[shortlist==1, prolific_id])
fluency_data= fluency_data[!prolific_id %in% bad_ids,]
# find all the words not in the scheme
questionable= fluency_data[!items %in% schemefile$word, items]
known_errors= spellfile[incorrect %in% questionable, incorrect]
unknown_errors= questionable[!questionable %in% spellfile$incorrect]
# newspelling= data.table(correct= rep(NaN, length(unknown_errors)), incorrect= sort(unknown_errors))
newspelling= fread("newspelling.csv")
spellfile= rbind(spellfile,newspelling[correct %in% spellfile$correct])
spellfile= spellfile[spellfile[, order(correct),],]
appendtoscheme= data.table(scheme= rep("TBA", length(spellfile[!correct %in% schemefile$word, correct])), word= spellfile[!correct %in% schemefile$word, correct])
schemefile= rbind(schemefile, appendtoscheme)
oldspellfile= fread("animals_snafu_scheme.csv")
oldscheme= fread("animals_snafu_scheme.csv")
# fwrite(schemefile,'updatedsnafuscheme.csv')
# fwrite(spellfile,'updatedsnafuspelling.csv')
colnames(oldscheme) <- c('scheme', 'word')
# do it again 
spellfile= fread("updatedsnafuspelling.csv")
schemefile= fread("updatedsnafuscheme.csv" )
questionable= fluency_data[!items %in% schemefile$word, unique(items)]
known_errors= spellfile[incorrect %in% questionable, incorrect]
for (i in known_errors){
  # correct work index
  cw_idx= spellfile[(incorrect==i),correct][1]
  fluency_data[items== i, items:= cw_idx]
}
unknownerrors= data.table(correct= rep(NaN, length(questionable[!questionable %in% known_errors & !questionable %in% schemefile$word])), incorrect= questionable[!questionable %in% known_errors])
spellfile= rbind(spellfile, unknownerrors)
spellfile= spellfile[spellfile[, order(correct),],]

# fwrite(fluency_data,"cleanfluency.csv")                     

