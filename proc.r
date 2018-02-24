#proc.r
#sort out procedures and generate a wide table for procedure history

patientproc <- patientprocedures.csv

#convert the dates
patientproc$Procedure.Date <- as.Date(patientproc$Procedure.Date , "%m/%d/%Y %H:%M:%S")

#need to see relative to the time of starting radiation though
#rttime from the genpatient code to generate start and end dates
temp <- left_join(patientproc, rttime, by="Patient.Identifier")

#now some timing things
temp$daysprior <- temp$start - temp$Procedure.Date

#filter to the past
temp <- filter(temp, daysprior > -1)

#now consolidate the different codes; level 1 seems too broad; the subcategory or level 2 or 3 seem good
#use level 3 for now since i think that's a good start
wideprocicd <- select(temp, Patient.Identifier, course, AHRQ.Procedure.Level.3.Text)
wideprocicd <- distinct(wideprocicd) #drop duplicates
wideprocicd <- filter(wideprocicd, AHRQ.Procedure.Level.3.Text != "")

#go from long to wide format
library(tidyr)
wideprocicd$present <- 1
wideprocicd <- spread(wideprocicd, AHRQ.Procedure.Level.3.Text, present)
wideprocicd[is.na(wideprocicd)] <- 0 #need to fill in the NAs

rm(temp) #cleanup

names(wideprocicd)[3:length(names(wideprocicd))] <- 
  paste0("proc_", names(wideprocicd)[3:length(names(wideprocicd))])