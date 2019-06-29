#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#combine.R
#combines all data; final data prep step

library(dplyr)

###combining data!
finalset <- NULL
finalset <- rtcourse

#calc the age
finalset$age <- as.numeric((finalset$start - finalset$Patient.Date.of.Birth)/365.25)

#need to filter the final set encounters by age (takes us to 8535 courses)
finalset <- filter(finalset, age >= 18)

#add on zipcodes
finalset <- left_join(finalset, pataddressinfo, by = "Patient.Identifier")

#add on admission data
modadmit <- ungroup(modadmit)
finalset <- left_join(finalset, modadmit, by = c("Patient.Identifier", "course"))

#add on ED data
moded <- ungroup(moded)
finalset <- left_join(finalset, moded, by = c("Patient.Identifier", "course"))

#make an admission or ED visit field
finalset$adm_ed <- 0 #initialize
finalset$adm_ed[finalset$ed == 1 | finalset$admit == 1] <- 1

#add concurrent and recent systemic therapy as classes and agents
#finalset <- left_join(finalset, anyneo, by = c("Patient.Identifier", "course"))
finalset <- left_join(finalset, widerecagent, by = c("Patient.Identifier", "course"))
finalset <- left_join(finalset, wideconagent, by = c("Patient.Identifier", "course"))
finalset <- left_join(finalset, widerecclass, by = c("Patient.Identifier", "course"))
finalset <- left_join(finalset, wideconclass, by = c("Patient.Identifier", "course"))

#add on the PMH data from the wide PMH file
finalset <- left_join(finalset, widepmhsubch, by = c("Patient.Identifier", "course")) #code for subchapters

#add on med classes concurrent and recent
finalset <- left_join(finalset, rxmixclasses, by = c("Patient.Identifier", "course")) # 295 classes
finalset <- left_join(finalset, recrxmixclasses, by = c("Patient.Identifier", "course")) # 298 classes

#add on abnormal labs and labs
finalset <- left_join(finalset, wideabnlabs, by = c("Patient.Identifier", "course")) #737

#add on icd/cpt procedure codes
finalset <- left_join(finalset, wideprocicd, by = c("Patient.Identifier", "course")) #323
finalset <- left_join(finalset, widecpt, by = c("Patient.Identifier", "course")) #9236

#add on social history
finalset <- left_join(finalset, social, by = c("Patient.Identifier", "course"))

#add on relevant vitals
finalset <- left_join(finalset, abnvitals, by = c("Patient.Identifier", "course"))

#add on EHR data evaluation
#finalset <- left_join(finalset, priorenc, by = c("Patient.Identifier", "course"))

#get rid of the column all NAs
finalset <- finalset[colSums(!is.na(finalset)) > 0]

#if you need to find any missing values
#M <- sapply(finalset, function(x) sum(is.na(x))); M[M>0]

#fill in the nas
finalset[is.na(finalset)] <- 0

#eliminate patients who were not discharged before the end of their RT; takes us to 8134
finalset <- filter(finalset, notdisch == 0)
finalset <- select(finalset, -notdisch) #then remove column

#fix the names
names(finalset) <- make.names(names(finalset), unique = TRUE)

#let's write the finalset file out to csv for saving
setwd("U:/TARS/")
write.csv(finalset, file = "finalset.csv")
save(finalset, file="finalset.Rda")
