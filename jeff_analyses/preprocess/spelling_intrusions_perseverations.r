library(data.table)

dat <- fread("../data/results.csv")

# FIX SPELLING

snafu_scheme <- fread("../misc/snafu_scheme.csv")
snafu_spelling <- fread("../misc/snafu_spelling.csv")

known_animals <- sort(unique(snafu_scheme[,word]))

for (i in 1:nrow(dat)) {
    animal <- dat[i, item]
    if (!(animal %in% known_animals)) { 
        if (animal %in% snafu_spelling$incorrect) {
            correct_animal <- snafu_spelling[incorrect==animal,correct]
            dat[i, item := correct_animal]
        }
    }
} 

# REMOVE INTRUSIONS 
# (including uncorrectable spelling errors + multi-items e.g. liontiger)

dat <- dat[(item %in% known_animals)]

# CODE FOR PERSEVERATIONS

perseverations <- dat[,.(firstitemposition=min(itemnum),.N),by=.(id,listnum,item)][N>1]

dat <- merge(dat, perseverations, all.x=T)
dat[(itemnum != firstitemposition) & !(is.na(N)), perseveration := 1]
dat[is.na(perseveration), perseveration := 0]
dat[,N := NULL]
dat[,firstitemposition := NULL]

# write data with and without perseverations
fwrite(dat, "../data/results_clean.csv", row.names=F, quote=F)
fwrite(dat[perseveration==0], "../data/results_noperseveration.csv", row.names=F, quote=F)

# write list 1 and list 2 separately for input to forager
fwrite(dat[listnum==1, .(id, entry=item)], "../data/results_l1.csv", row.names=F, quote=F, sep="\t")
fwrite(dat[listnum==2, .(id, entry=item)], "../data/results_l2.csv", row.names=F, quote=F, sep="\t")

