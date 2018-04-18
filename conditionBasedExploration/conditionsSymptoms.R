
# Pull in the data --------------------------------------------------------
conditionsSymptoms <- fd[trackable_type %in% c("Symptom", "Condition")]


# Pre-processing ----------------------------------------------------------
## Preliminary cleaning
conditionsSymptoms <- conditionsSymptoms[grepl(x= trackable_name, pattern = "[^A-Za-z\\s]+")]
conditionsSymptoms[, trackable_name := tolower(trackable_name)]


# Processing --------------------------------------------------------------
conditionsSymptoms[, countByCondtion := .N, by = trackable_name]
conditionsSymptoms[, uniqueUser := uniqueN(user_id), by = trackable_name]
conditionsSymptoms <- unique(conditionsSymptoms[, .(countByCondtion, uniqueUser), by = trackable_name])
stop()
## Get outliers by user and condition count and then set aside from main data set 
outliers <- conditionsSymptoms[uniqueUser < .001*uniqueN(fd$user_id) & countByCondtion < .001*uniqueN(fd$trackable_name)]
conditionsSymptoms <- conditionsSymptoms[!(uniqueUser < .001*uniqueN(fd$user_id) & countByCondtion < .001*uniqueN(fd$trackable_name))]
outlitersByCondition <- conditionsSymptoms
spell_check_files()


cs <- conditionsSymptoms[,.(trackable_name)]
cs <- unique(cs)
cs <- cs[1:5]
install.packages("RecordLinkage")
library(RecordLinkage)


    
   test <- function(var){
    cs[trackable_name == var,prob := lapply(conditionsSymptoms$trackable_name, function(x) levenshteinSim(x, cs[trackable_name != x, trackable_name]))]
     }

   lapply(cs$trackable_name, test)
