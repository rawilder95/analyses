rm(list= ls())
library(data.table)
library(ggplot2)
library(gridExtra)
library(ez)
library(cowplot)
setwd('~/Downloads/jatos_mac_java/analyses')


dat= fread('fluency_data_07152023.csv')
# N words listed trial by delay by age
d1= dat[ ,.N, by= .(prolific_id, age, condition, listnum)]
d1[, prolific_id:= factor(prolific_id)]
d1[, age:= factor(age)]
d1[, listnum:= factor(listnum)]
d1[, condition:= factor(condition)]
# ANOVA for N items listed
#Warning:Data is unbalanced (unequal N per group).
#Same for listnum= 173, age: old= 180, young= 166, condition: immediate = 186, delayed = 160
ezANOVA(d1, within=listnum, between=c("age","condition"), dv=N, wid=prolific_id)

# Repeated words from l1 to l2
repeated_words= dat[, .N, .(prolific_id,items)][N>2,items]
dat[, repeated:= 0]
dat[listnum==2 & items %in% repeated_words, repeated:= 1]
geom_bar()


