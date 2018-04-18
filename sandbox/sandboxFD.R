setwd("/Users/cbayer/Desktop")
fd <- read.csv(file = "export.csv", stringsAsFactors = F)
library(data.table)
library(ggplot2)
library(tidyverse)
library(stats)
library(shiny)
library(shinydashboard)
fd <- data.table(fd)
fd[, uniqueN(user_id)]
fd[, trackable_value := as.numeric(trackable_value)]
fd[, trackable_name := tolower(trackable_name)]
fd[, checkin_date := as.Date(checkin_date)]

#fd <- fd[trackable_name == "migraine" | trackable_name == "headache" | trackable_name == "head ache" | trackable_name == "rheumatoid arthritis" | trackable_name == "pressure"]
fd <- fd[][order(user_id, checkin_date)]
fd[trackable_name == "pressure", pressureDelta := trackable_value - shift(trackable_value), 
   by = .(user_id)]

fd[, maxPain := 0]
fd[trackable_name != "pressure" & between(trackable_value,left = -1, right = 5), 
   maxPain := max(trackable_value, na.rm = T), 
   by = .(user_id, checkin_date)]
fd[trackable_name != "pressure", meanPain := mean(trackable_value, na.rm = T), by = user_id]
fd2 <- fd[, .(pressureDelta, maxPain, meanPain), by = .(user_id, checkin_date)] 
fd3 <- fd[trackable_name== "pressure", .(user_id, checkin_date, trackVal = trackable_value)]

setkey(fd3, user_id, checkin_date)
setkey(fd2, user_id, checkin_date)
fd4 <- fd3[fd2]
fd4 <- unique(fd4)
fd4 <- fd4[!(is.na(pressureDelta) & is.na(maxPain))]
fd4$trackVal <- as.integer(fd4$trackVal)
fd5 <- fd4[!(is.na(trackVal) | is.na(maxPain))]
cor(fd5$trackVal, fd5$maxPain)

fd5[, pressureDelta := trackVal - shift(trackVal)]
fd5[is.na(pressureDelta), pressureDelta := 0]
fd5[, absoluteDelta := abs(pressureDelta)]
fd5[is.na(absoluteDelta), absoluteDelta := 0]

## Get sub lists of users by condition
migraines <- fd[trackable_name == "migraine" | trackable_name == "migrane", user_id]
headaches <- fd[trackable_name == "headache" | trackable_name == "head ache", user_id]
rheum <- fd[trackable_name == "rheumatoid arthritis", user_id]
arth <- fd[grepl(x = trackable_name, pattern = "arth"), user_id]

fd5[is.na(fd5)] <- 0
cor(fd5$pressureDelta, fd5$maxPain)
cor(fd5$absoluteDelta, fd5$maxPain)

lm1 <- lm(data = fd5,formula = maxPain ~ absoluteDelta + meanPain )
lm2 <- lm(data = fd5,formula = maxPain ~ pressureDelta + meanPain)
lm3 <- lm(data = fd5,formula = maxPain ~ trackVal)
lm4 <- lm(data = fd5[user_id %in% migraines],formula = maxPain ~ absoluteDelta + meanPain )
lm5 <- lm(data = fd5[user_id %in% headaches],formula = maxPain ~ absoluteDelta + meanPain )
lm6 <- lm(data = fd5[user_id %in% rheum],formula = maxPain ~ absoluteDelta + meanPain )

summary(lm1)
summary(lm2)
summary(lm3)

summary(lm4)
summary(lm5)
summary(lm6)


plot(lm1)
fd[trackable_type == "Weather", .N, by = trackable_name]
fd2 <- fd[, .(dates= uniqueN(checkin_date)), by = user_id]
fd2[, mean(dates, na.rm = T)]
d <- density(x = fd2$dates)
plot(d)
fd2[, freq := .N, by = dates]
fd2[, bins := "0"]
fd2[dates <= 10, bins := "1-10 days"]
fd2[dates > 10 & dates <= 30, bins := "11-30 days"]
fd2[dates > 31 & dates <= 60, bins := "31-60 days"]
fd2[dates > 61 & dates <= 180, bins := "61-180 days"]
fd2[dates > 181 & dates <= 365, bins := "180-365 days"]
fd2[dates > 366, bins := "365 days+"]

fd[, checkin_date := as.Date(checkin_date)]
fd[, minByUser := min(checkin_date, na.rm = T), by = user_id]
fd[, maxByUser := max(checkin_date, na.rm = T), by = user_id]
fd[, span := as.integer(maxByUser-minByUser), by = user_id]
fdRetention <- unique(fd[, .(minByUser, maxByUser, span), by = user_id])
summary(fdRetention$span)

fd[checkin_date < "2015-01-15", .N]
fd[checkin_date < "2015-01-15", uniqueN(user_id)]

fd2[, freq2 := .N, by = bins]
g <- ggplot(fd2, aes(x= bins, y = freq2)) +
  geom_bar(stat = "identity") 

fd[, trackable_name := to_lower(trackable_name)]


# dcast -------------------------------------------------------------------
fd <- read.csv(file = "export.csv", stringsAsFactors = F)
fd <- data.table(fd)
fd[, uniqueN(user_id)]
fd[, trackable_value := as.numeric(trackable_value)]
fd[, trackable_name := tolower(trackable_name)]
dcast(data = fd[trackable_type=="Weather"], 
      formula = user_id + checkin_date ~ trackable_name,
      value.var = "trackable_value", fun.aggregate = mean)


# temp --------------------------------------------------------------------
fd[, avgWeather := as.integer(0)]
fd[trackable_name %in% c("temperature_min", "temperature_max"), avgWeather := mean(trackable_value), by = .(user_id, checkin_date)]
fd[, avgWeather := avgWeather/2]

fd[, avgPain := 0]
fd[!is.na(trackable_value), avgPain := mean((trackable_value[trackable_type == "Condition" | 
                                        trackable_type == "Symptom"])), 
   by = .(user_id, checkin_date)]

fd[, avgWeather := as.integer(0)]
fd[trackable_name == "temperature_min" | trackable_name ==  "temperature_min", avgWeather := sum(trackable_value, na.rm = T), by = .(user_id, checkin_date)]

fd6 <- fd[, .(avgWeather, avgPain), by = .(user_id, checkin_date)]
fd6 <- unique(fd6)
fd6[is.na(avgPain), avgPain := 0]
cor(fd6$avgWeather, fd6$avgPain)

fd7 <- fd6[user_id %in% arth, ]
cor(fd7$avgWeather, fd7$avgPain)

lm0 <- lm(data = fd7[avgWeather != 0],formula = avgPain ~ avgWeather)
summary(lm0)

lm00 <- lm(data = fd6[avgWeather != 0],formula = avgPain ~ avgWeather)
summary(lm00)


# Dry/Itchy skin ----------------------------------------------------------
skinUsers <- fd[(grepl(x =trackable_name, pattern = "skin") &
                   (grepl(x =trackable_name, pattern = "itchy") |
                      grepl(x =trackable_name, pattern = "dry")) & 
                   trackable_type %in% c("Condition", "Symptom")), user_id]

skin <- fd[(grepl(x =trackable_name, pattern = "skin") &
              (grepl(x =trackable_name, pattern = "itchy") |
                 grepl(x =trackable_name, pattern = "dry")) & 
              trackable_type %in% c("Condition", "Symptom")) & user_id %in% skinUsers | trackable_type == "Weather"]

skin[trackable_type %in% c("Condition", "Symptom"), 
     avgPain := mean(trackable_value, na.rm = T), by = .(user_id, checkin_date)]

skin[, avgPrecip := 0]
skin[trackable_name == "precip_intensity" & !is.na(trackable_value), avgPrecip := max(trackable_value, na.rm = T), by = .(user_id, checkin_date)]
skin[trackable_name == "humidity", avgHumid := max(trackable_value), by = .(user_id, checkin_date)]
skin <- unique(skin[!is.na(avgPain) |!is.na(avgHumid) | !is.na(avgPrecip), .(avgPain, 
                                                                      avgPrecip = max(avgPrecip, na.rm = T), 
                                                                      avgHumid = max(avgHumid, na.rm = T)), 
             by = .(user_id, checkin_date)])

skin
lm(data = skin, formula = avgPain ~ avgPrecip)
lm(data = skin, formula = avgPain ~ avgHumid)

## find average pain time period
# First find the max pain by user
fd[, maxPain := 0]
fd[trackable_type %in% c("Condition", "Symptom"), maxPain := max(trackable_value, na.rm = T), by = .(user_id)]
# make binary for any time user pain is between max and 75% percentile of pain (maybe 60)
fd[, highPain := 0]
fd[between(x = avgPain,lower = .60*maxPain, maxPain), highPain := 1]
fd[, lowPain := 0]
fd[highPain == 0, lowPain := 1]

# find days between high pain
fd[, consecutiveDays := 0]
fd <- fd[order(user_id, checkin_date)]
fd[, checkin_date := as.Date(checkin_date)]
fd[, consecutiveDays := checkin_date - shift(checkin_date), by = user_id]
fdPeriodicity <- unique(fd[,.(user_id, checkin_date, highPain, lowPain)])
fdPeriodicity <- fdPeriodicity[order(user_id, checkin_date)]
fdPeriodicity[, counter := 0]
fdPeriodicity[, counter := as.numeric(seq_len(.N)), by = .(user_id, rleid(highPain))]
fdPeriodicity[, counterBetweenFlares := 0]
fdPeriodicity[highPain ==0, counterBetweenFlares := counter]
fdPeriodicity[, counterDuringFlares := 0]
fdPeriodicity[highPain ==1, counterDuringFlares := counter]

# merge data back on
fdSubset <- unique(fd[trackable_type %in% c("Condition", "Symptom"),.(user_id, age, sex, checkin_date, avgPain, avgWeather)])
setkey(fdSubset, user_id, checkin_date)
setkey(fdPeriodicity, user_id, checkin_date)
fdPeriodicity <- fdPeriodicity[fdSubset]
fdPeriodicity <- fdPeriodicity[!is.na(avgPain) & !is.na(counter) & !is.na(counterBetweenFlares) & !is.na(counterDuringFlares)]

## relationships between timing and pain
cor(fdPeriodicity$avgPain, fdPeriodicity$counterBetweenFlares)
lm(data = fdPeriodicity, formula = avgPain ~ counterBetweenFlares)
cor(fdPeriodicity$avgPain, fdPeriodicity$counterDuringFlares)
lm(data = fdPeriodicity, formula = avgPain ~ counterDuringFlares)
