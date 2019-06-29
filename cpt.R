#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#cpt.r
#import cpt codes

patientcpt <- patientcptprocedures.csv

#convert the dates
patientcpt$CPT.Procedure.Date <- as.Date(patientcpt$CPT.Procedure.Date , "%m/%d/%Y %H:%M:%S")

#need to see relative to the time of starting radiation though
#rttime from the genpatient code to generate start and end dates
temp <- left_join(patientcpt, rttime, by="Patient.Identifier") #will increase #

#now some timing things
temp$daysprior <- temp$start - temp$CPT.Procedure.Date
#we need to get rid of any negative numbers
temp$daysprior[temp$daysprior < 0] <- NA

#filter to the past
temp <- filter(temp, daysprior > -1)

widecpt <- select(temp, Patient.Identifier, course, CPT.Short.Description)
widecpt <- distinct(widecpt)
widecpt <- filter(widecpt, CPT.Short.Description != "")

#go from long to wide format
library(tidyr)
widecpt$present <- 1
widecpt <- spread(widecpt, CPT.Short.Description, present)
widecpt[is.na(widecpt)] <- 0 #need to fill in the NAs


names(widecpt)[3:length(names(widecpt))] <-
  paste0("cpt_", names(widecpt)[3:length(names(widecpt))])
