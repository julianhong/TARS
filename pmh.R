#manage problem list and prior diagnoses
#there are two sources for past medical history; the problem list and prior ICD diagnoses

#first we want to get them in the same format:
#Patient identifier, plaintext name for readability, ICD-10 and an earliest date

library(dplyr)
problemlists <- problemlists.csv

#let's start with problemlists (populate this with all encounter problemlists)
problemlists <- select(problemlists, Patient.Identifier, Problem.Name, 
                       Current.List.of.ICD10.Diagnosis.Codes, Date.Problem.was.Noted,
                       Date.Problem.was.Entered) #there are two dates here

#now reformat the dates
problemlists$Date.Problem.was.Noted <- as.Date(problemlists$Date.Problem.was.Noted,
                                               "%m/%d/%Y %H:%M:%S")
problemlists$Date.Problem.was.Entered <- as.Date(problemlists$Date.Problem.was.Entered,
                                               "%m/%d/%Y %H:%M:%S")

#we really want when the problem was first appreciated, so we can take the date the problem
#was initially noted and if that's blank, take the date that it was entered.
problemlists$date <- problemlists$Date.Problem.was.Noted

problemlists$date[is.na(problemlists$date)] <-
  problemlists$Date.Problem.was.Entered[is.na(problemlists$date)]

library(icd)
library(magrittr)

#process the factor into a string
problemlists$Current.List.of.ICD10.Diagnosis.Codes<- 
  as.character(problemlists$Current.List.of.ICD10.Diagnosis.Codes)
#then separate out by comma and collapse the lists into vector form
temp <- do.call("rbind", strsplit(problemlists$Current.List.of.ICD10.Diagnosis.Codes, ", "))
colnames(temp) <- paste("Current.List.of.ICD10.Diagnosis.Codes", 1:ncol(temp), sep = "")
####NOTE THAT THIS WILL CREATE SOME DUPLICATES BUT WILL COLLAPSE OUT
#now rejoin the temporary data frame
problemlists <- bind_cols(problemlists, as.data.frame(temp))
rm(temp) #cleanup

#then drop out any unnecessary columns
problemlists <- select(problemlists, -Problem.Name, -Date.Problem.was.Noted, 
                       -Date.Problem.was.Entered, -Current.List.of.ICD10.Diagnosis.Codes)

#now we want to make sure the problem was added to the history PRIOR to RT start
#rttime from the genpatient code to generate start and end dates
problemlists <- left_join(problemlists, rttime, by="Patient.Identifier")

#now determine if the problem prior to RT
problemlists$existing <- problemlists$date <= problemlists$start
#now just pull the ones that were existing
problemlists <- filter(problemlists, existing == TRUE)

#then can remove the date and RT date fields and existing but make sure to keep the course #
problemlists <- select(problemlists, -date, -start, -end, -existing)

#now convert the problem list from a long to a wide format
#the icd library has issues with duplicate patient ids so just easier to use tidyr and gather
library(tidyr)
#convert the problem list into strings so they can be collapsed
problemlists$Current.List.of.ICD10.Diagnosis.Codes1 <- as.character(problemlists$Current.List.of.ICD10.Diagnosis.Codes1)
problemlists$Current.List.of.ICD10.Diagnosis.Codes2 <- as.character(problemlists$Current.List.of.ICD10.Diagnosis.Codes2)
problemlists$Current.List.of.ICD10.Diagnosis.Codes3 <- as.character(problemlists$Current.List.of.ICD10.Diagnosis.Codes3)
problemlists$Current.List.of.ICD10.Diagnosis.Codes4 <- as.character(problemlists$Current.List.of.ICD10.Diagnosis.Codes4)

longproblist<- gather(problemlists, "icdfield", "coicd10", 2:5) #the ICD columns

#get rid of duplicates to improve processing time
longproblist <- 
  longproblist %>%
  select(-icdfield) %>%
  distinct()

#we want the three_digit so substring it
longproblist$cothree_digit <- strtrim(longproblist$coicd10, 3)

#now implement icd_explain
problemicd<-longproblist
problemicd$explain <- sapply(problemicd$cothree_digit, icd_explain)

#there are some that are basically not mapped (E codes it seems); we'll just go ahead and make these NAs
problemicd$explain[problemicd$explain == "character(0)"] <- NA

#recast to flat vector
problemicd$explain <- unlist(problemicd$explain)

#we can also do this with subchapters; this has been moved to RTencounter and should be initialized already
#have to rename the subchapter field here
temp10 <- rename(icd10mapjh, icdsubch = sub_chapter, cothree_digit = three_digit)

#now we need to convert the long problem list into the subchapters in our dictionary that should be an easy left_join
library(dplyr)
temp<-left_join(longproblist, temp10, "cothree_digit")
rm(temp10) #cleanup
problemsubch <- distinct(select(temp, Patient.Identifier, course, icdsubch)) #filter down to the distinct per course; watch out that you also have the RT subchapter here
problemsubch <- filter(problemsubch, !is.na(icdsubch))
rm(temp) #cleanup



##############
#now let's work on patientdiagnoses
patientdiagnoses <- patientdiagnoses.csv

patientdiagnoses <- select(patientdiagnoses, Patient.Identifier, Diagnosis.Name, 
                           ICD.Diagnosis.Code, ICD.Diagnosis.Code.Set, Diagnosis.Date)
#reformat the date
patientdiagnoses$Diagnosis.Date <- as.Date(patientdiagnoses$Diagnosis.Date, "%m/%d/%Y %H:%M:%S")
#the issue here is the ICD coding which can be of various sources

#same deal wth the date filters here
#now we want to make sure the problem was added to the history PRIOR to RT start
#rttime from the genpatient code to generate start and end dates
patientdiagnoses <- left_join(patientdiagnoses, rttime, by="Patient.Identifier")

#now determine if the problem prior to RT
patientdiagnoses$existing <- patientdiagnoses$Diagnosis.Date <= patientdiagnoses$start
#now just pull the ones that were existing
patientdiagnoses <- filter(patientdiagnoses, existing == TRUE)

#drop unnecessary columns
patientdiagnoses <- select(patientdiagnoses, -Diagnosis.Date, -Diagnosis.Name
                           , -start, -end, -existing)

#get rid of patients who have no ICD diagnosis code for comorbidity
patientdiagnoses <- filter(patientdiagnoses, ICD.Diagnosis.Code != "")
patientdiagnoses <- distinct(patientdiagnoses) #and get rid of duplicates


#we'll break up the ICD-9 and ICD-10
#we'll lose some codes here (custom)?
patientdiagnoses10 <- filter(patientdiagnoses, ICD.Diagnosis.Code.Set == "ICD-10-CM")
patientdiagnoses9 <- filter(patientdiagnoses, ICD.Diagnosis.Code.Set == "ICD-9-CM")

#can drop the set type here
patientdiagnoses10 <- select(patientdiagnoses10, -ICD.Diagnosis.Code.Set)
patientdiagnoses9 <- select(patientdiagnoses9, -ICD.Diagnosis.Code.Set)

#start with icd10
#we want the three_digit so substring it
patientdiagnoses10$cothree_digit <- strtrim(patientdiagnoses10$ICD.Diagnosis.Code, 3)

#now use icd_explain
ptdiag10icd<-patientdiagnoses10
ptdiag10icd$explain <- sapply(ptdiag10icd$cothree_digit, icd_explain)

#flatten out explain
temp <- NULL
temp <- rapply(ptdiag10icd$explain, function(x) ifelse(length(x)==0,NA,x), how = "replace")
ptdiag10icd$explain <- unlist(temp, use.names = FALSE)
rm(temp)

#there are some that are basically not mapped (E codes it seems); we'll just go ahead and make these NAs
ptdiag10icd$explain[ptdiag10icd$explain == "character(0)"] <- NA

#similarly let's also do this with a subchapter file
library(dplyr)
#have to rename the subchapter field here
temp10 <- rename(icd10mapjh, icdsubch = sub_chapter, cothree_digit = three_digit)
ptdiagsubch10<-left_join(patientdiagnoses10, temp10, "cothree_digit") #note that this will actually extend the list because some codes map to mult subchapters
ptdiagsubch10 <- filter(ptdiagsubch10, !is.na(icdsubch))
rm(temp10) #cleanup

#now onto icd 9
#convert from icd 9 to 10
temp1 <- patientdiagnoses9

temp1$conodec <- sapply(strsplit(gsub("[.]", "", temp1$ICD.Diagnosis.Code), " ,"), "[", 1)

temptoicd10 <- rename(toicd10, conodec = nodec, coicd10 = icd10)

#so we had made the translation with the RT data so let's use that
#start with a left join
temp <- left_join(temp1, temptoicd10, by = "conodec")

#then some of the nas are 3 digits so they need a fourth digit buffer with a 0
temp$conodec[(nchar(temp$conodec) == 3 & is.na(temp$coicd10))] <-
  paste(temp$conodec[(nchar(temp$conodec) == 3 & is.na(temp$coicd10))], "0", sep = "")

#then rejoin
temp <- left_join(temp, temptoicd10, by = "conodec")

#then the ones leftover look like 4 digits that need a fifth digit buffer with a 0
temp$conodec[(nchar(temp$conodec) == 4 & is.na(temp$coicd10.y))] <-
  paste(temp$conodec[(nchar(temp$conodec) == 4 & is.na(temp$coicd10.y))], "0", sep = "")

#then rejoin
temp <- left_join(temp, temptoicd10, by = "conodec")

#there are also a handful of codes that instead ofa 0 you can stick a 1 in for the last digit that are 4 or 5
temp$conodec[(nchar(temp$conodec) >3 & is.na(temp$coicd10))] <-
  paste(substr(temp$conodec[(nchar(temp$conodec) > 3 & is.na(temp$coicd10))], 1, 
         nchar(temp$conodec[(nchar(temp$conodec) > 3 & is.na(temp$coicd10))])-1),"9", sep = "")

#then rejoin
temp <- left_join(temp, temptoicd10, by = "conodec")

temp1 <- select(temp, -coicd10.x, -coicd10.y, -coicd10.x.x)
temp1 <- rename(temp1, coicd10 = coicd10.y.y)

#have to lose things that are NA in coicd10
temp1 <- filter(temp1, !is.na(coicd10))


rm(temp, temptoicd10)

patientdiagnoses9 <- temp1
rm(temp1)


#the icd 9 key was already made before for the RT diagnoses
patientdiagnoses9$cothree_digit <- strtrim(patientdiagnoses9$coicd10, 3)

#and now explain
ptdiag9icd<-patientdiagnoses9 #i kept the old version that was built on icd9 alone

ptdiag9icd$explain <- sapply(ptdiag9icd$cothree_digit, icd_explain) #this command can take like 21 hrs to run

#recast to flat vector
temp <- NULL
temp <- rapply(ptdiag9icd$explain, function(x) ifelse(length(x)==0,NA,x), how = "replace")
ptdiag9icd$explain <- unlist(temp, use.names = FALSE)
rm(temp)

#there are some that are basically not mapped (E codes it seems); we'll just go ahead and make these NAs
ptdiag9icd$explain[ptdiag9icd$explain == "character(0)"] <- NA

 
#now do this with the key
#have to rename the subchapter field here
temp <- rename(icd10mapjh, icdsubch = sub_chapter, cothree_digit = three_digit)
#another join here too
ptdiagsubch9<-left_join(patientdiagnoses9, temp, "cothree_digit") #note that this will actually extend the list because some codes map to mult subchapters
ptdiagsubch9 <- filter(ptdiagsubch9, !is.na(icdsubch))
rm(temp)


#bring it all together
pmhicd <- distinct(bind_rows(select(problemicd, Patient.Identifier, course, explain), 
                             select(ptdiag9icd, Patient.Identifier, course, explain), 
                             select(ptdiag10icd, Patient.Identifier, course, explain)))
#ditto for the subchapter form
pmhsubch <- distinct(bind_rows(select(problemsubch, Patient.Identifier, course, icdsubch), 
                               select(ptdiagsubch9, Patient.Identifier, course, icdsubch), 
                               select(ptdiagsubch10, Patient.Identifier, course, icdsubch)))

#get rid of NAs in pmhicd
pmhicd <- filter(pmhicd, !is.na(explain))

#now we need it in wide format again
pmhicd$comorbid <- 1 #dummy variable
library(tidyr)
library(dplyr)
widepmhicd <- spread(pmhicd, explain, comorbid) #get rid of the NAs

#make all the NAs 0
widepmhicd[is.na(widepmhicd)] <- 0

#same procedure for subchapters
pmhsubch$comorbid <- 1 #dummy variable
library(tidyr)
widepmhsubch <- spread(filter(pmhsubch, !is.na(icdsubch)), icdsubch, comorbid)
widepmhsubch[is.na(widepmhsubch)] <- 0