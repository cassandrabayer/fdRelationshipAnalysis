# Objective 

# Find conditions that are commonly tracked together
## Where condition == x, 

fd[, countsSymptoms := ]

userInfo <- unique(fd[, .(age, 
                          sex, 
                          country,
                          daysTrackedTotal = uniqueN(checkin_date),
                          conditionSymptom = paste(unique(trackable_name[ trackable_type=="Condition" | trackable_type=="Symptom"][order(trackable_name)]), collapse = ", "),
                          tags = paste(trackable_name[ trackable_type=="Tag"], collapse = ", ")),
                      by = user_id])


# Information about longest streak of tracking
alldates <- data.table(checkin_date=seq.Date(min(fd$checkin_date), max(fd$checkin_date), by="day"))
fdDates <- merge(fd, alldates, by="checkin_date", all=TRUE)
fdDates <- unique(fdDates[,][order(user_id, checkin_date)]) 
fdDates[!is.na(user_id), date := 1:nrow(fdDates)]
fdDates[!is.na(user_id), diff := checkin_date - shift(checkin_date)]