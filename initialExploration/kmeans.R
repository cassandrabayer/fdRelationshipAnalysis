fdFull <- fd
fd <- fdFull[trackable_type %in% c("Condition", "Symptom")]

medDict <- read.table("wordlist.txt", encoding = "UTF-8")
medDict <- data.table(medDict)
fd[, trackableNameCount := .N, by = trackable_name]

library(stringr)
bodyParts <- c("(body)|(feet)|(foot)|(toes)|(toe)|(hand)|(hands)|(head)|(elbow)|(arm)|(arms)|(neck)|(shoulder)|(abdomen)|(stomach)|(hip)|(hips)|(stomach)|(ankle)|
               (ankles)|(leg)|(legs)|(jaw)|(jaws)")

fd[nwords(string = trackable_name)>3 & 
     grepl(x=trackable_name, pattern = "pain") & 
     grepl(x= trackable_name, pattern = bodyParts), 
   trackable_name := paste0(str_extract(string = trackable_name,pattern = bodyParts), " pain")]
fd[, trackableNameCount := .N, by = trackable_name]
fd[][order(trackableNameCount)]
outliers <- fd[trackableNameCount<10, .(trackableNameCount), by = trackable_name]
outliers <- unique(outliers)
fd <- fd[trackableNameCount>10, .(trackableNameCount), by = trackable_name]


dictionary(add_words = medDict$V1,affix = "wordlist.txt")

fd[, spellcheck := hunspell_check(trackable_name)]
fd[spellcheck == F, badWords := hunspell_find(trackable_name)]
fd[spellcheck == F & badWords %in% medDict$V1]
fd[badWords != "", suggestedWords :=  lapply(seq_along(badWords), function(x) hunspell_suggest(badWords[[x]]))]
fd[badWords != "", analyze := lapply(seq_along(badWords), hunspell_analyze)]

dictionary


## Exploring caffeine and headaches
fd <- fdFull[(trackable_type %in% c("Condition", "Symptom")) | 
               (grepl(x = trackable_name, pattern = "caffeine", ignore.case = T )) |
               (grepl(x = trackable_name, pattern = "coffee", ignore.case = T ))]

fd[, .N, by = trackable_name]
fd[trackable_type %in% c("Condition", "Symptom") & trackable_name %in% c("headache", "migraine"), ]
fd[, caffeineDay := 0]
fd[(grepl(x = trackable_name, pattern = "caffeine", ignore.case = T )) |
     (grepl(x = trackable_name, pattern = "coffee", ignore.case = T )), caffeineDay := 1, by = checkin_date]
fd[caffeineDay ==1]
fd[trackable_type %in% c("Condition", "Symptom"), avgPain := mean(trackable_value, na.rm = T), by = .(user_id, checkin_date)]
caffeine <- unique(fd[, .(avgPain, caffeineDay), by = .(user_id, checkin_date)])
caffeine[caffeineDay==1 & !is.na(avgPain)]
lm <- lm(formula = avgPain ~ caffeineDay, data = caffeine)
summary(lm)



hunspell_suggest(unlist(fd[3]$trackable_name))


### KMeans for grouping text
library(RecordLinkage)
