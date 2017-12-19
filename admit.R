#admit.R
#find admissions and ED visits in DEDUCE encounters data

#convert the encounter dates
encounter$Admit.Date <- as.Date(encounter$Admit.Date, "%m/%d/%Y %H:%M:%S")
encounter$Discharge.Date <- as.Date(encounter$Discharge.Date, "%m/%d/%Y %H:%M:%S")
encounter$Arrival.Date <- as.Date(encounter$Arrival.Date, "%m/%d/%Y %H:%M:%S")
encounter$ED.Arrival.Date <- as.Date(encounter$ED.Arrival.Date, "%m/%d/%Y %H:%M:%S")

#clean up so we have an admission table and an ED table
#admissions have an admit date and time
admissions <- filter(select(encounter, Patient.Identifier, Admit.Date, 
                     Total.Hospital.LOS.in.days, Discharge.Date, MS.DRG.Description), !is.na(Admit.Date))
ed <- filter(select(encounter, Patient.Identifier, ED.Arrival.Date), !is.na(ED.Arrival.Date))


#admission within the RT dates?
#do a left join of RT/admission first
temp2 <- left_join(rttime, admissions, by = "Patient.Identifier") #rt course has to come first here to make sure we duplicate the admissions
#now determine if the admission was in the RT period; 1/0

#11/5/17: mark this only as true if not a chemo admission (to eliminate planned admissions)
temp2$admit <- 1* (temp2$Admit.Date > temp2$start & temp2$Admit.Date <= temp2$end & 
                     !grepl("CHEMO", temp2$MS.DRG.Description)) #start counting at day 2

#while we're here we can also pull the time to their last discharge
temp2$daysprior <- temp2$start - temp2$Discharge.Date
#we need to get rid of any negative numbers
temp2$daysprior[temp2$daysprior < 0] <- NA

#now calculate out admitted at start
temp2$admitatstart <- 0
temp2$admitatstart[(temp2$start >= temp2$Admit.Date) & (temp2$start < temp2$Discharge.Date)] <- 1 #include admitted day 1

#calc out discharge during RT if admitted at start
temp2$notdisch <- 0
temp2$notdisch[temp2$admitatstart == 1] <- 1* !(temp2$Discharge.Date[temp2$admitatstart == 1] >= 
                                                   temp2$start[temp2$admitatstart == 1] & 
                                                   temp2$Discharge.Date[temp2$admitatstart == 1] < 
                                                   temp2$end[temp2$admitatstart == 1])

#now calculate discharge from an admission within last year?
temp2$lastyear <- 0 #initialize
temp2$lastyear[temp2$daysprior < 366] <- 1

#admissions in the past month?
temp2$lastmonth <- 0 #initialize
temp2$lastmonth[temp2$daysprior < 31] <- 1

#calc out admitted days in the last year
temp2$lastyrdays <- temp2$lastyear * temp2$Total.Hospital.LOS.in.days

#make a list of reasons for admission for reference
temp <- filter(temp2, admit == 1)
reasonforadmit <- table(temp$MS.DRG.Description)

#now consolidate for each course
temp2 <- group_by(temp2, Patient.Identifier, course)
modadmit <- summarize(temp2, admit = max(admit), lastadmit = as.numeric(min(daysprior, na.rm = TRUE)), 
                      admitatstart = max(admitatstart), notdisch = max(notdisch),
                      numyearadmit = sum(lastyear, na.rm = TRUE),
                      nummonthadmit = sum(lastmonth, na.rm = TRUE), daysyearadmit = sum(lastyrdays, na.rm = TRUE))

#because of the join, fields in admit and admitatstart and numyearadmit can become NAs- let's set those to 0
#also inf in last admission so we should change this to a finite number
modadmit$lastadmit[is.infinite(modadmit$lastadmit)] <- 10^100 #replace with a large finite number
modadmit$admit[is.na(modadmit$admit)] <- 0
modadmit$admitatstart[is.na(modadmit$admitatstart)] <- 0
modadmit$numyearadmit[is.na(modadmit$numyearadmit)] <- 0
modadmit$numyearadmit[is.na(modadmit$nummonthadmit)] <- 0
modadmit$numyearadmit[is.na(modadmit$daysyearadmit)] <- 0

rm(temp, temp2) #cleanup

####similar process for ED visits
#need to find ED visits
temp2 <- left_join(rttime, ed, by = "Patient.Identifier")
#now determine if the admission was in the RT period 1/0
temp2$ed <- 1 * (temp2$ED.Arrival.Date > temp2$start & temp2$ED.Arrival.Date <= temp2$end)

#while we're here we can also pull the time to their last ED visit
temp2$daysprior <- temp2$start - temp2$ED.Arrival.Date
#we need to get rid of any negative numbers
temp2$daysprior[temp2$daysprior < 0] <- NA

#now calculate ED within last month?
temp2$lastmonth <- 0 #initialize
temp2$lastmonth[temp2$daysprior < 31] <- 1

#ED within the last year?
temp2$lastyear <-0 #initialize
temp2$lastyear[temp2$daysprior < 366] <- 1

#now consolidate for each course
temp2 <- group_by(temp2, Patient.Identifier, course)
moded <- summarize(temp2, ed = max(ed), lasted = as.numeric(min(daysprior, na.rm = TRUE)), 
                      nummonthed = sum(lastmonth, na.rm = TRUE), numyeared = sum(lastyear, na.rm = TRUE))

#because of the join, fields can become NAs- let's set those to 0
#also inf in last ED so we should change this
moded$lasted[is.infinite(moded$lasted)] <- 10^100 #replace with a large finite number
moded$ed[is.na(moded$ed)] <- 0
moded$nummonthed[is.na(moded$nummonthed)] <- 0
moded$numyeared[is.na(moded$numyeared)] <- 0

rm(temp2) #cleanup
