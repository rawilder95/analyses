# This file should not be run by itself, it is called from json_to_csv.r

json_data_immediate <- readLines("../data/results_immediate.json")

# NOTE: Not all data is moved to the CSV! (e.g., firsttime, distractor data)

# IMMEDIATE CONDITION
numlines <- length(json_data_immediate)
parser <- newJSONParser()
for (i in 1:numlines) {
    parser$addData(json_data_immediate[i])
    GOOD_PARTICIPANT <- TRUE

    grab_fluency_list <- function() {
        # only add data if it's a valid list (>=5 items). by ignoring shorter lists, this means that some participants may be on their 'second try'...
        num_responses <- 0
        while (num_responses < 5) {
            fluency <- parser$getObject()
            
            # check for edge case where participant has zero valid lists
            if (!is.null(fluency)) {
                num_responses <- length(fluency$items)
            } else {
                GOOD_PARTICIPANT <<- FALSE # 
                break
            }
        }
        return(fluency)
    }

    fluency1 <- grab_fluency_list()

    if (GOOD_PARTICIPANT) { distractor <- parser$getObject() }

    fluency2 <- grab_fluency_list()
 
    if (GOOD_PARTICIPANT) { demographics <- parser$getObject() }
    
    if (GOOD_PARTICIPANT) {
        num_responses_l1 <- length(fluency1$items)
        num_responses_l2 <- length(fluency2$items)

        list1_data <- data.table(id = rep(fluency1$prolific_id, num_responses_l1),
                               gender = rep(demographics$gender, num_responses_l1),
                               age = rep(demographics$ageText, num_responses_l1),
                               condition = rep(demographics$condition, num_responses_l1),
                               listnum = rep(fluency1$gamenum, num_responses_l1),
                               itemnum = seq(1, num_responses_l1),
                               item = fluency1$items,
                               time = fluency1$times[1:num_responses_l1],
                               starttime = rep(fluency1$starttime, num_responses_l1))

        list2_data <- data.table(id = rep(demographics$prolific_id, num_responses_l2),
                               gender = rep(demographics$gender, num_responses_l2),
                               age = rep(demographics$ageText, num_responses_l2),
                               condition = rep(demographics$condition, num_responses_l2),
                               listnum = rep(fluency2$gamenum, num_responses_l2),
                               itemnum = seq(1, num_responses_l2),
                               item = fluency2$items,
                               time = fluency2$times[1:num_responses_l2],
                               starttime = rep(fluency2$starttime, num_responses_l2))
       
        list_data <- rbind(list1_data, list2_data)

        if (!exists("immediate_data")) {
            immediate_data <- list_data # only for the very first list in the file
        } else {
            immediate_data <- rbind(immediate_data, list_data)
        }
    }
}

# Fix two participant ids
immediate_data[id == "5f402eb4fbd8df145068b8055f402eb4fbd8df145068b805", id := "5f402eb4fbd8df145068b805"]
immediate_data[id == "622a2074fe80192aa7a0212a622a2074fe80192aa7a0212a", id := "622a2074fe80192aa7a0212a"]

