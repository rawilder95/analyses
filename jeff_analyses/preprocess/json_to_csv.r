rm(list=ls())

library(data.table)
library(rjson)
library(stringr)

source('json_to_csv_immediate.r')
source('json_to_csv_delayed.r')

dat <- rbind(immediate_data, delayed_data)

# Basic clean-up
dat[, item := tolower(item)]
dat[, item := str_replace_all(item, " ", "")]
dat[, item := str_replace_all(item, "[[:punct:]]", "")]
dat[, age := as.numeric(age)]

# one 59 year old but let's count them since prolific did the screening for us
dat[age < 30, age_group := "young"]
dat[age > 30, age_group := "old"]

# add RTs
dat[, rt := time - shift(time), by = .(id, listnum)]
dat[itemnum==1, rt := time - starttime]

# participant listed lots of instruments
dat <- dat[id != "611c0958e3975a30a658a3fe"]

fwrite(dat, "../data/results.csv")
