# Project Title

Brief description of what the project does.

## Table of Contents

## Note: If you are running this script for the first time or changes have been made to the datafile, 
## run/re-run all of the scripts in order to ensure that each file being used is updated as well.
## Instructions
- Delayed Condition raw data: results_delayed.txt
# cleaned using clean_delayedcondition.R
- "jatos_id": Identifier provided by JATOS, works in place of prolific_id     - "prolific_id": Identifier provided by prolific website (participant recruitment). Is used as subject ID but can be swapped out with jatos_id. - "condition": Identifier for whether the participant completed the immediate or delayed condition    
- "itemnum": Ordinal position of responses per trial      - "items": Responses for repeated fluency        - "times": Unix timestamp        - "firsttime": Response time between first key press and response submission    - "category": Semantic fluency category completed for given trial.     - "starttime": Time the trial started.  Used to calculate response times from "times".- "shortlist": Did participants list 5 words or less? If so, exclude them from analysis.    - "rt": Response times.           - "gender": Participant demographics       - "genderText": Participant demographics   - "ageText": Participant demographics        - "didYouTry": Participant demographics    - "understand": Participant demographics   - "comments": Participant demographics        - "listnum"/"gamenum": Col identifier for trial 1 versus trial 2. Original JSON file uses column identifier gamenum which is relabeled to "listnum" in initial cleaning script.        - "age": Participant demographics              
- Immediate Condition raw data: results_delayed.txt
#cleaned using clean_immediatecondition.R
-"equations": Immediate only distractor task responses.  Task presents participants with simple arithmetic problems to solve for 60 seconds.
#Both the clean_immediate/delayedcondition.R scripts convert the raw JSON file into a csv.  The immediate condition has a distractor trial and the delay needs to have it's condition col identifier switched to "Delayed", so it's easier to do the initial cleaning separately
- Run the cleandata_current.R script after running the clean_immediatecondition.R/clean_delayedcondition.R files.
#This script binds both the immediate and delayed condition together
#Removes whitespace
#Sets plural items to singular case
#Sets all items to lowercase
#Spellchecks the file against SNAFU scheme and spell files
- Run perseveration
#this finds the number of perseveration errors as a function of col identifiers: condition, age, listnum
#the following variables are added to the main data.table in this script
-"word_counts": How many times was this word listed by this participant in this trial?  - "perseveration": Is this response perseveration? Calculated by indexing all instances where an item was repeated by the same participant, within a trial and excluding the first instance where it was listed.

## Configuration
- for script cleandata_current.R use 'immediatefluency_merged.csv' and 'delayedfluency_merged.csv'
- for script perseverationcode.R use 'combined fluency.csv'
- for script responselength_repetitions.R use 'fluency_noerror.csv'
- for script custeranalysis.R use 'fluencynoerror.csv' (i.e. semantic fluency no spelling or perseveration errors) and 'clusterswitches.csv'

## Usage




## Contributing
(Optional) Guidelines for contributing to the project, submitting pull requests, and the issue tracking process.

## Testing
(Ask Jeff about listing sanity checks here and instructions)

## License
Clearly state the project's license.

## Acknowledgments
- Kumar, A.A., Apsel, M., Zhang, L., Xing, N., Jones. M.N. (2023). forager: A Python package and web interface for modeling mental search.

- Kumar, A.A., Lundin, N. B., Jones, M.N. (2022). Mouse-mole-vole: The inconspicuous benefits of phonology during retrieval from semantic memory. In Proceedings of the 44th Annual Meeting of the Cognitive Science Society.

-Hills, T. T., Jones, M. N., & Todd, P. M. (2012). Optimal foraging in semantic memory. Psychological Review, 119(2), 431.

## Contact
(Optional) Provide contact information for users or developers to reach out for questions or feedback.

