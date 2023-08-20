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
setwd("~/Downloads/jatos_mac_java/analyses")
f1_trial = function(var){
  data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), items= my_data$items, times= as.numeric(my_data$times), firsttime= my_data$firsttime[1:numrows], category= rep( my_data$category, numrows), starttime= rep(my_data$starttime, numrows), shortlist= rep(0, numrows))
  }
dist_trial= function(var){
  data.table(jatos_id= rep(my_data$jatos_id, numrows), prolific_id= rep( my_data$prolific_id,numrows), gamenum= rep(my_data$gamenum, numrows), itemnum= seq(1,numrows), equations= my_data$equations, times= my_data$times, firsttime= my_data$firsttime[1:numrows], starttime= rep(my_data$starttime, numrows), responses= my_data$responses)
  }
files <- list.files(pattern= "*.txt")
dat= data.table()
json_string <- read_file("results_immediate.txt")
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
      print('demographics')
    } else{
      numrows= length(my_data$equations)
      distractor= dist_trial()
      print("distractor")
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
# Add in the manual stuff that got messed up with the code
fluency_data[, condition:= "Immediate"]
fluency_data$times <- as.numeric(fluency_data$times)
dist_data[, condition:= "Immediate"]
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
# dist_data <- dist_data[!sapply(dist_data$list_column, is.null), on = .(jatos_id, gamenum)]
str(fluency_data)
fluency_data[, rt:= times-shift(times,1), by= .(jatos_id, gamenum)]
fluency_data[itemnum==1, rt:= times-unique(starttime)]
fluency_data[, rt:= rt/1000]
fluency_data
fluency_data[, sum(rt), by= gamenum]
dist_data = dist_data[!(is.na(jatos_id)),]
fwrite(fluency_data, "temp_files/immediate_fluency.csv")
fwrite(demo_data, "temp_files/immediate_demographics.csv")
fwrite(dist_data, "temp_files/immediate_distractor.csv")
dist_data[, equations:= as.character(equations)]
j= merge(fluency_data, demo_data)
fwrite(j, "temp_files/immediatefluency_merged.csv")








