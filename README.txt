# Project Title

Brief description of what the project does.

## Table of Contents
- [Instructions](#instructions)
- [Usage](#usage)
- [Configuration](#configuration)
- [Contributing](#contributing)
- [Testing](#testing)
- [License](#license)
- [Acknowledgments](#acknowledgments)
- [Contact](#contact)

## Note: If you are running this script for the first time or changes have been made to the datafile, 
## run/re-run all of the scripts in order to ensure that each file being used is updated as well.
## Instructions
- Delayed Condition raw data: results_immediate.txt
# cleaned using clean_delayedcondition.R
- "jatos_id": Identifier provided by JATOS, works in place of prolific_id     
- "itemnum": Ordinal position of responses per trial      
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
-"word_counts": How many times was this word listed by this participant in this trial?  



## Usage
- 

## Configuration
[how to customize project]

## Contributing
(Optional) Guidelines for contributing to the project, submitting pull requests, and the issue tracking process.

## Testing
(Ask Jeff about listing sanity checks here and instructions)

## License
Clearly state the project's license.

## Acknowledgments
(Optional) Give credit to those whose work your project builds upon.

## Contact
(Optional) Provide contact information for users or developers to reach out for questions or feedback.
