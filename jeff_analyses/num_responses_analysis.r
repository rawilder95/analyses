library(data.table) 
library(ez)
library(gridExtra)
source("misc/basicplot.r")

dat <- fread("data/results_noperseveration.csv")
dat[, perseveration := NULL]
dat[, listnum := factor(listnum)]

item_level <- dat[,.(num_responses = .N), by=.(id, condition, age_group, listnum)]


# data
item_level[,.(num_responses = mean(num_responses)),by=.(age_group, condition, listnum)]

# inferential
ezANOVA(wid=id, data=item_level, dv=num_responses, between=c("age_group","condition"), within=listnum)

# plot
a <- bbar(item_level[age_group == "young",num_responses, by=.(condition, listnum)]) + ylim(0,30) + ggtitle("YOUNG")
b <- bbar(item_level[age_group == "old",num_responses, by=.(condition, listnum)]) + ylim(0,30) +ggtitle("OLD")
grid.arrange(a, b, nrow=1)

bbar(item_level[,num_responses, by=.(condition, listnum)])

# age_group and condition*listnum are significant

