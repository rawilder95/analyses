# This file should not be run by itself, it is called from json_to_csv.r

json_data_delayed <- readLines("../data/results_delayed.json")

# NOTE: Not all data is moved to the CSV! (e.g., firsttime)

numlines <- length(json_data_delayed)
parser <- newJSONParser()
for (i in 1:numlines) {
    parser$addData(json_data_delayed[i])
    GOOD_PARTICIPANT <- TRUE

    # only add data if it's a valid list (>=5 items). by ignoring shorter lists, this means that some participants may be on their 'second try'...
    num_responses <- 0
    while (num_responses < 5) {
        fluency <- parser$getObject()
        
        # check for edge case where participant has zero valid lists
        if (!is.null(fluency)) {
            num_responses <- length(fluency$items)
        } else {
            GOOD_PARTICIPANT <- FALSE # 2 participants
            break
        }
    }


    if (GOOD_PARTICIPANT) {
        demographics <- parser$getObject()
        list_data <- data.table(id = rep(fluency$prolific_id, num_responses),
                               gender = rep(demographics$gender, num_responses),
                               age = rep(demographics$ageText, num_responses),
                               condition = rep(demographics$condition, num_responses),
                               listnum = rep(fluency$gamenum, num_responses),
                               itemnum = seq(1, num_responses),
                               item = fluency$items,
                               time = fluency$times[1:num_responses],
                               starttime = rep(fluency$starttime, num_responses))

        if (!exists("delayed_data")) {
            delayed_data <- list_data # only for the very first list in the file
        } else {
            delayed_data <- rbind(delayed_data, list_data, fill=TRUE) 
            # fill=TRUE because one list (id=63e8326b8d067b28655586fb) is
            # missing demographics. could take demographics from other list,
            # but it turns out they need to be removed anyway (list is all
            # instruments). code below removes the participant for 'having one
            # list'
        }
    }
}

# Remove participants who only have one list (did not come back for 2nd trial)
# Also remove one participant with three lists (2 lists on day one)
bad_ids <- delayed_data[itemnum==1, .N, by=id][N==1, id]
bad_ids <- c(bad_ids, delayed_data[itemnum==1, .N, by=id][N==3, id])
delayed_data <- delayed_data[!(id %in% bad_ids)]

# Re-code listnums; for delayed condition everything is "list 1" in in the JSON
delayed_data[,firstlist := (starttime == min(starttime)), by=id]
delayed_data[firstlist == FALSE, listnum := 2]
delayed_data[,firstlist := NULL]

# One participant has mis-matched age between trials, but in both cases is an older adult
# Manually change age from 52 to 62 because 52 year-olds weren't eligible! assumed a typo
delayed_data[id=="640cf44e8bf4e101d82a76a1", age := max(age)]
