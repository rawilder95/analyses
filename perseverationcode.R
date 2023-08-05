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
dat= fread('fluency_data_07152023.csv')
# dat= fread('fluencydata_cleaned062223.csv')



f1_trial = function(var){
  data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), items= my_data$items, times= as.numeric(my_data$times), firsttime= my_data$firsttime[1:numrows], category= rep( my_data$category, numrows), starttime= rep(my_data$starttime, numrows), shortlist= rep(0, numrows))
}
dist_trial= function(var){
  data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), equations= my_data$equations, times= my_data$times, firsttime= my_data$firsttime[1:numrows], starttime= rep(my_data$starttime, numrows), responses= my_data$responses)
}


dat= data.table()
json_string <- read_file("rawdataImmediate.txt")

str_len= length(json_string)
fluency_data= data.table()
dist_data= data.table()
demo_data= data.table()
this_obj= json_string
parser <- newJSONParser()
parser$addData(this_obj)
my_data <- parser$getObject()
while (!is.null(my_data$prolific_id)){
  numrows= length(my_data$items)
  if(is.null(my_data$items)){
    if(is.null(my_data$equations)){
      demo_data <- rbind(demo_data, as.data.table(my_data), fill= TRUE)
    } else{
      numrows= length(my_data$equations)
      distractor= dist_trial()
      dist_data = rbind(dist_data, distractor, fill= TRUE)
    }
  } else {
    d1 <- f1_trial(my_data)
    if(length(my_data$items) <=5){
      d1[,shortlist:= 1]
      print('shortlist!')
    } else{
      d1[, shortlist:=0]
    }
    fluency_data<- rbind(fluency_data, d1,fill= TRUE)
  }
  my_data <- parser$getObject()
}
#Add in condition
fluency_data[, condition:= "Immediate"]
# Set to numeric value
fluency_data$times <- as.numeric(fluency_data$times)
dist_data[, condition:= "Immediate"] #for distractor
fluency_data[, rt:= times-shift(times,1), by= .(jatos_id, gamenum)]
fluency_data[itemnum==1, rt:= times-unique(starttime)]
fluency_data[itemnum==1, rt:= times-unique(starttime)]
fluency_data[, rt:= rt/1000]
dist_data$equations <- as.character(dist_data$equations)
fluency_data <- fluency_data[!is.na(prolific_id),]
fluency_data[is.na(prolific_id)]
fluency_data$items <- sapply(fluency_data$items, as.character)
dist_data$equations <- sapply(dist_data$equations, as.character)
fluency_data[, items:= unlist(items)]
dist_data[, equations:= unlist(equations)]
# Replace col game num with col listnum
dat[, listnum:= gamenum]
dat= subset(dat, select= -c(gamenum))
dat= merge(fluency_data,demo_data)
# change the name so that you can keep the dt and replicate the function for delayed
immediate_dt= dat

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

