# MAKE FUNCTION: d1= data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), items= my_data$items, times= my_data$times, firsttime= my_data$firsttime[1:numrows], category= rep( my_data$category, numrows), starttime= rep(my_data$starttime, numrows), shortlist= rep(0, numrows))
# dat= rbind(dat,d1, fill= TRUE)
options(scipen = 999)
rm(list= ls())
library(data.table)
library(rngtools)
library(rjson)
library(jsonlite)
library(RJSONIO)
library(readr)
# Define the which_condition function
# setwd('~/Downloads/jatos_mac_java/analyses') # JCZ
f1_trial = function(var){
  data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), items= my_data$items, times= as.numeric(my_data$times), firsttime= my_data$firsttime[1:numrows], category= rep( my_data$category, numrows), starttime= rep(my_data$starttime, numrows), shortlist= rep(0, numrows))
}
dat= data.table()
json_string <- read_file("results_delayed.txt")
str_len= length(json_string)
fluency_data= data.table()
demo_data= data.table()
parser <- newJSONParser()
parser$addData(json_string)
my_data <- parser$getObject()
while (!is.null(my_data$prolific_id)){
  numrows= length(my_data$items)
  if(is.null(my_data$items)){
    demo_data <- rbind(demo_data, as.data.table(my_data), fill= TRUE)
  } else {
    d1 <- f1_trial(my_data)
    if(length(my_data$items) <=5){
      d1[,shortlist:= 1]
      } else{
      d1[, shortlist:=0]
    }
    fluency_data<- rbind(fluency_data, d1,fill= TRUE)
  }
  my_data <- parser$getObject()
}
# Add in the manual stuff that got messed up with the code
fluency_data[prolific_id== "5fc5b0ec967b5f13a297562a5fc5b0ec967b5f13a297562a"]$prolific_id <- "5fc5b0ec967b5f13a297562a"
fluency_data= fluency_data[!is.na(prolific_id)]
fluency_data$times <- as.numeric(fluency_data$times)
fluency_data$items <- sapply(fluency_data$items, as.character)
fluency_data[, rt:= times-shift(times,1), by= .(prolific_id, gamenum)]
fluency_data[is.na(rt), rt:= 0]
fluency_data[itemnum==1, rt:= times-unique(starttime)]
fluency_data[, rt:= rt/1000]
fluency_data
# Create a new column 'game_order' to represent the order of gamenum values based on starttime
# Sort the data by prolific_id and starttime
fluency_data[, game_order:= as.numeric(starttime== max(starttime)), by= (prolific_id)]
fluency_data[game_order==1, game_order:= 2]
fluency_data[game_order==0, game_order:= 1]
# Perform the rearrangement of gamenum values
for (i in unique(fluency_data$prolific_id)) {
  this_subj <- fluency_data[prolific_id == i]
  if (sum(this_subj$game_order == 2) > 0) {
    this_subj[starttime == min(this_subj[starttime == max(starttime), starttime]), game_order := 1]
    this_subj[starttime == max(this_subj[starttime == min(starttime), starttime]), game_order := 2]
  }
  fluency_data[prolific_id == i] <- this_subj
}
# Get rid of unnecessary game_order category after setting gamenum
fluency_data[, gamenum:= game_order]
fluency_data= subset(fluency_data, select= -c(game_order))
# double check that each participant did both trials
# Fix javascript code error
demo_data[, condition:= "Delayed"]
fwrite(fluency_data, "delayed_fluency.csv")
fwrite(demo_data, "delayed_demographics.csv")
j= merge(fluency_data, demo_data)
# Fix javascript error for fluency 
fluency_data[, condition:= "Delayed"]
fwrite(j, "delayedfluency_merged.csv")
k=fread('delayedfluency_merged.csv')



