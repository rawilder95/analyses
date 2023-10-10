library(ez)
library(data.table)
library(gridExtra)
source("misc/basicplot.r") # bbar

dat <- fread("data/results_clean.csv")
dat[, listnum := factor(listnum)]

perseverations <- dat[perseveration==1,.(num_perseverations=.N),by=.(id,listnum)]
perseverations <- merge(unique(dat[,.(id, age_group, condition, listnum)]), perseverations, all.x=T)
perseverations[is.na(num_perseverations),num_perseverations := 0]

perseverations[,mean(num_perseverations),by=.(age_group, condition, listnum)]



# data
perseverations[,.(num_perseverations = mean(num_perseverations)),by=.(age_group, condition, listnum)]

# inferential
ezANOVA(wid=id, data=perseverations, dv=num_perseverations, between=c("age_group","condition"), within=listnum)

# plot
a <- bbar(perseverations[age_group == "young",num_perseverations, by=.(condition, listnum)]) + ggtitle("YOUNG") + ylim(0,1) + theme_classic()
b <- bbar(perseverations[age_group == "old",num_perseverations, by=.(condition, listnum)]) + ggtitle("OLD") + ylim(0,1) + theme_classic()
grid.arrange(a, b, nrow=1)

# only listnum is significant
