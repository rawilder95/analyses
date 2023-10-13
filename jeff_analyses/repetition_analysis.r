library(data.table)
library(ez)
library(gridExtra)
source("misc/basicplot.r")

dat <- fread("data/results_noperseveration.csv")
dat[, perseveration := NULL]
dat[, listnum := factor(listnum)]

# code repeats only in list 2
item_level_repeats <- dat[,.(repeated = .N), by=.(id, item)][repeated==2]
item_level_repeats <- merge(dat, item_level_repeats, all.x=T)
item_level_repeats[listnum==1, repeated := NA]
item_level_repeats[listnum==2 & is.na(repeated), repeated := 0]
item_level_repeats[listnum==2 & repeated==2, repeated := 1]

repeats <- item_level_repeats[listnum==2, .(num_responses = .N, num_repeats = sum(repeated)), by=.(id, age_group, condition)]
repeats[,proportion_repeated := num_repeats / num_responses]

# data
repeats[,.(proportion_repeated = mean(proportion_repeated)), by=.(age_group, condition)]

# inferential
ezANOVA(wid=id, data=repeats, dv=proportion_repeated, between=c("age_group","condition"))

# plot
bbar(repeats[, proportion_repeated, by=.(age_group, condition)])

# no significant results

