#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#RTimport.R
#imports Aria reports with original RT plan data

#Import the basic patient info
setwd("U:/TARS/source/report")
files <- list.files(pattern = '*.csv')

for (i in 1:length(files)) {
  assign(files[i], read.csv(files[i]))
}
rm(files) #ariareport.csv

ariareport <- ariareport.csv

#manual review to get a final list

#manage the changedMrns
library(dplyr)
changedMrns <- distinct(bind_rows(changedMrns20132014.csv, changedMrns20152016.csv))
names(changedMrns)[1] <- "PatientId"

#recode the date
ariareport$start <- as.Date(ariareport$X1st.Day, "%m/%d/%Y %H:%M")
ariareport$end <- as.Date(ariareport$Last.Day, "%m/%d/%Y %H:%M")

library(dplyr)
ariareport <-
  ariareport %>%
  filter(Machine.Full.Name != "TBCary" & Machine.Full.Name != "TBMacon" &
           Machine.Full.Name != "2100DRH" &
           Machine.Full.Name != "21DHRH" &
           Machine.Full.Name != "TBRaleigh" &
           Machine.Full.Name != "" &
           Machine.Full.Name != "21MPMC") %>% #exclude satellites
  rename(PatientId = Pt.ID) %>%
  distinct() %>% #all unique
  filter(Delivered.Dose > 0) %>%
  select(-X1st.Day, -Last.Day) %>%
  group_by(PatientId, Course.ID) %>%
  summarize(numplans = n(), planneddose = sum(Planned.Dose), plannedfx = sum(Planned.Fractions), #cumulative for all sites/fields
            fx = sum(Delivered.Fractions), dose = sum(Delivered.Dose), revisions = max(Revision..),
            start = min(start), end = max(end)) %>%
  filter(end > as.Date("2012-12-31") & start < as.Date("2017-01-01")) #need to filter time window

#these are old MRNs so we should link back to the changed MRNs too
ariareport <- left_join(ariareport, changedMrns)
ariareport$New.MRN <- as.character(ariareport$New.MRN)
ariareport$New.MRN[is.na(ariareport$New.MRN)] <- ariareport$PatientId[is.na(ariareport$New.MRN)]
ariareport$Duke.MRN <- ariareport$New.MRN
ariareport <- select(ungroup(ariareport), -New.MRN, -PatientId)

#reviewing the billing codes shows that those without courses are either:
#satellite patients who were treated for isolated treatments when machines were down
#or brachy patients

#####now bring in the diagnosis/procedure data
#replace MRNs in the RT encounters with the new ones
#let's do this as a join
library(dplyr)

allRT <- bind_rows(RT2013.csv, RT2014.csv, RT2015.csv, RT2016.csv, RTadd.csv) #146415 entries

edgefx <- bind_rows(RT2012.csv, RT2017.csv) #inclusion is treatment from 2013-2016; some  may start/finish before/after
temp <- unique(allRT$PatientId) #pull all of our patients in the cohort
temp2 <- filter(edgefx, is.element(PatientId, temp)) #pull out the extra fractions

#the extra fractions may represent discrete treatments, but we will figure this out after we tabulate courses
allRT <- bind_rows(allRT, temp2) #should total 151462 (all distinct)
rm(temp, temp2)

allRT <- left_join(allRT, changedMrns)
allRT$New.MRN <- as.character(allRT$New.MRN)
allRT$New.MRN[is.na(allRT$New.MRN)] <- allRT$PatientId[is.na(allRT$New.MRN)]
allRT$PatientId <- allRT$New.MRN
allRT$New.MRN <- NULL

#recode the date
allRT$ToDateOfService <- as.Date(allRT$ToDateOfService, "%m/%d/%Y")

#bring in the patient identifier now
names(allRT)[1] <- "Duke.MRN"
temp <- left_join(allRT, select(patient, Duke.MRN, Patient.Identifier), by = "Duke.MRN") #manually review
#than those used in DEDUCE so sometimes instead of updating you actually have to backdate to an older MRN
#to make sure things match (verified by JH)
#lets just drop the Duke MRN now that we're recoded

#one missing MRN which will be dropped (would not be included due to exclusion criteria anyway)
sum(is.na(temp$Patient.Identifier))
temp <- na.omit(temp)

allRT <- temp #now 151460
rm(temp)

#remove any non-RT codes; 151346
allRT <-
  allRT %>%
  filter(regexpr("marker", allRT$ProcedureCodeDescription) == -1 &
           regexpr("Ultrasound", allRT$ProcedureCodeDescription) == -1)


#recode the treatments
#SRS-SBRT
allRT$srssbrt[allRT$ProcedureCodeDescription == "Robotic SRS -SBRT Radiation Therapy- 1st treatment" |
                regexpr("Stereo", allRT$ProcedureCodeDescription) != -1 |
                allRT$ProcedureCodeDescription == "Steriotactic Body / SRT cranial  course not to exceed 5 fractions"] <- 1
allRT$srssbrt[is.na(allRT$srssbrt)] <- 0
#2D3D
allRT$conv[regexpr("Treatment Delivery", allRT$ProcedureCodeDescription) != -1] <- 1
allRT$conv[is.na(allRT$conv)] <- 0
#IMRTVMAT
allRT$imrtvmat[allRT$ProcedureCodeDescription == "Intensity modulated treatm delivery, single or multiple fields" |
                 allRT$ProcedureCodeDescription == "IMRT- Rapid Arc"] <- 1
allRT$imrtvmat[is.na(allRT$imrtvmat)] <- 0
#Brachy, can do this for anything containing "Brachy" or "HDR"
allRT$brachy[regexpr("Brachy", allRT$ProcedureCodeDescription) != -1 |
               regexpr("HDR", allRT$ProcedureCodeDescription) != -1 |
               regexpr("Insertion", allRT$ProcedureCodeDescription) != -1 |
               regexpr("Interstitial", allRT$ProcedureCodeDescription) != -1 |
               regexpr("marker", allRT$ProcedureCodeDescription) != -1] <- 1 #seems like an insertion procedure
allRT$brachy[is.na(allRT$brachy)] <- 0
#TBI
allRT$tbi[allRT$ProcedureCodeDescription == "Total Body Photon Irradiation"] <- 1
allRT$tbi[is.na(allRT$tbi)] <- 0
#TSI
allRT$tsi[allRT$ProcedureCodeDescription == "Total Body Electron Irradiation"] <- 1
allRT$tsi[allRT$ProcedureCodeDescription == "Total body -  Electron"] <- 1
allRT$tsi[is.na(allRT$tsi)] <- 0

#now we need to incorporate this data into the courses we have in the aria report
#we can do this by matching the course ID
#the NAs post join are all either coverage treatments from satelllites or brachy (which are excluded)

RTencounter <-
  ariareport %>%
  left_join(allRT, by = "Duke.MRN") #184909 entries here

#if the date is within the course, make a course column that is equal to the course id as a label
#note that there were a couple of discrepancies on date by a day that had to be manually fixed in the RT file
RTencounter$course <- NULL
RTencounter$course[RTencounter$ToDateOfService >= RTencounter$start & RTencounter$ToDateOfService <= RTencounter$end] <-
  as.character(RTencounter$Course.ID[RTencounter$ToDateOfService >= RTencounter$start & RTencounter$ToDateOfService <= RTencounter$end])
RTencounter <- filter(RTencounter, !is.na(course))

#fix diagnosis codes
#find out max diagnoses
i <- max(sapply(strsplit(as.character(RTencounter$DiagnosisId), ' ,'), length))
library(tidyr)
temp <-
  RTencounter %>%
  #separate it out but you will get a warning about too few
  separate(DiagnosisId, paste0("icd", 1:i), sep = " ,") %>%
  group_by(Duke.MRN, ToDateOfService) %>%
  #turn it into one column ***important used 4 columns here based on i makes row for each - 573200 rows
  gather("icdnum", "icd", icd1, icd2, icd3, icd4)

#we can make a course diagnosis table with the course # and the diagnoses
coursedx <-
  temp %>% group_by(Patient.Identifier, course) %>%
  distinct(Patient.Identifier, course, icd) %>%
  filter(!(is.na(icd) | icd == "")) %>% #clean up blanks/NA
  rename(nodec = icd) #rename for the data dictionary
#strip that decimal point
coursedx$nodec <- sapply(strsplit(gsub("[.]", "", coursedx$nodec), " ,"), "[", 1) #10659 entries
rm(temp)


#reformat the diagnosis into a binary table
#have to start by unifying and translating codes

#let's unify to ICD10
#start by creating the conversion dictionary from the CDC GEM file
#probably makes the most sense to go from 10 to 9 since most are 9
setwd("U:/TARS/source/")
toicd10 <- select(read.table("CDCGEM/2018_I9gemNCHSCDC9to10.txt",strip.white = TRUE, colClasses = "character",
                             col.names = c("nodec", "icd10", "flags")), -flags)
#note that JH added some codes to top of the file to help with ambiguities in the set; there are still NAs left
#but they are for Z29 (other prophylactic encounters)

#for simplicity we only want the first entry though by icd10
toicd10 <- group_by(toicd10, nodec)
toicd10 <- summarise(toicd10, icd10 = first(icd10))

#so we have to do this staggered due to the nature of the key
#start with a left join
temp <- left_join(coursedx, toicd10, by = "nodec")

#then some of the nas are 3 digits so they need a fourth digit buffer with a 0
temp$nodec[(nchar(temp$nodec) == 3 & is.na(temp$icd10))] <-
  paste(temp$nodec[(nchar(temp$nodec) == 3 & is.na(temp$icd10))], "0", sep = "")

#then rejoin
temp <- left_join(temp, toicd10, by = "nodec")

#then the ones leftover look like 4 digits that need a fifth digit buffer with a 0
temp$nodec[(nchar(temp$nodec) == 4 & is.na(temp$icd10.y))] <-
  paste(temp$nodec[(nchar(temp$nodec) == 4 & is.na(temp$icd10.y))], "0", sep = "")

#then rejoin
temp <- left_join(temp, toicd10, by = "nodec")

#now fill in all of the codes that were icd10
temp$icd10[is.na(temp$icd10)] <- temp$nodec[is.na(temp$icd10)]

coursedx <- select(temp, -icd10.x, -icd10.y)
rm(temp)

#reformat the icd code to the first 3 digits
#EXCEPT C79 which should be left full
coursedx$three_digit[substr(coursedx$icd10, 1,3) != "C79"] <- substr(coursedx$icd10, 1,3)[substr(coursedx$icd10, 1,3) != "C79"]
coursedx$three_digit[substr(coursedx$icd10, 1,3) == "C79"] <- coursedx$icd10[substr(coursedx$icd10, 1,3) == "C79"]

#also bc of the # of diagnoses, let's also do the subchapters (only need icd 10 right now but set up the maps for other segments)
#we'll make our maps here - will also make a map for the short descriptions
library(icd)
icd9mapjh <-distinct(select(icd9cm_hierarchy, three_digit, sub_chapter)) #want just the three-digit and the sub_chapter
icd10mapjh <-distinct(select(icd10cm2016, three_digit, sub_chapter)) #want just the three-digit and the sub_chapter
icd9mapdxjh <-rename(distinct(select(icd9cm_hierarchy, code, short_desc)), icd9 = code) #want the 3 digit, desc, and subch
icd10mapdxjh <-rename(distinct(select(icd10cm2016, code, short_desc)), icd10 = code) #want the 3 digit, desc, and subch

#need to correct C79 which is miscoded
icd10mapjh$sub_chapter <- as.character(icd10mapjh$sub_chapter)
icd10mapjh$sub_chapter[icd10mapjh$three_digit == "C79"] <-
  "Secondary malignant neoplasm of other and unspecified sites"
icd10mapjh$sub_chapter <- as.factor(icd10mapjh$sub_chapter)

#for the sake of simplicity narrow to the first entry (only needed for icd 10 - 9 is good)
temp <- group_by(icd10mapjh, three_digit)
temp <- summarise(temp, sub_chapter = first(sub_chapter))
icd10mapjh <- temp
rm(temp)

#start with icd explain
library(icd)
temp <- coursedx
temp$dx <- sapply(coursedx$three_digit, icd_explain)
#a handful of codes C79s just need their trailing 0s removed
temp$dx[as.character(temp$dx) == "character(0)"] <- sapply(substr(temp$three_digit[as.character(temp$dx) == "character(0)"], 1, 4), icd_explain)
temp$dx <- as.character(temp$dx)
#the last ones are 3 digit codes so we should be able to pull that out of the icd10 dictionary
tempkey <- rename(icd10mapdxjh, three_digit = icd10)
temp1 <- left_join(temp, tempkey, by = "three_digit")
temp1$dx[temp1$dx == "character(0)"] <- temp1$short_desc[temp1$dx == "character(0)"]
#last step is NAs which are all Z29 (prophylactic measures)
temp1$dx[is.na(temp1$dx)] <- "Prophylactic measures" #anytime big coding changes you should check!!
temp1$dx <- as.factor(temp1$dx) #make factor
temp2 <- distinct(select(temp1, Patient.Identifier, course, dx)) #distinct now that grouped by 3 digit
temp2$present <- 1

#so now we have just diagnoses (three digits) so we should turn this into a binary table
library(reshape2)
widecoursedx <- dcast(temp2, Patient.Identifier + course ~ dx)
#fill in with 0s
widecoursedx[is.na(widecoursedx)] <- 0
rm(temp1, temp2)
#add prefix since we have other dx-based columns
names(widecoursedx)[3:length(names(widecoursedx))] <-
  paste0("coursedx_", names(widecoursedx)[3:length(names(widecoursedx))])

#we also want to do the same thing with subchapters
#we can just build off of temp which we will now modify to truly be 3 digits for everything
temp$three_digit <- substr(temp$icd10, 1,3)
temp1 <- left_join(temp, icd10mapjh, by = "three_digit")
#last step is NAs which are all Z29 (prophylactic measures)
temp1$sub_chapter <- as.character(temp1$sub_chapter) #recast
temp1$sub_chapter[is.na(temp1$sub_chapter)] <- "Prophylactic measures" #anytime big coding changes you should check!!
temp2 <- distinct(select(temp1, Patient.Identifier, course, sub_chapter)) #distinct now that grouped by 3 digit
temp2$present <- 1
temp2$sub_chapter <- as.factor(temp2$sub_chapter) #recast
widecoursesubch <- dcast(temp2, Patient.Identifier + course ~ sub_chapter)
#fill in with 0s
widecoursesubch[is.na(widecoursesubch)] <- 0
rm(temp1, temp2)
#add prefix since we have other dx-based columns
names(widecoursesubch)[3:length(names(widecoursesubch))] <-
  paste0("coursesub_", names(widecoursesubch)[3:length(names(widecoursesubch))])


#alright now we have to reformat into courses and condense
#group by the patient, course
temp <- group_by(RTencounter, Patient.Identifier, course)
#now use summarise to get us back to our course format
#everything was calced at the beginning so we just need to get back to that (so max/min will be selecting from same values)
rttime <- summarise(temp, start = min(start), end= max(end),
                    attending = names(which.max(table(AttendingOncologistName))), planneddose = max(planneddose),
                    plannedfx = max(plannedfx), fx = max(fx), dose = max(dose),
                    numcharge = n(), srssbrt = max(srssbrt), conv = max(conv), imrtvmat = max(imrtvmat),
                    brachy = max(brachy), tbi = max(tbi), tsi = max(tsi)) #back to 8892 courses as in ariareport
rm(temp)

#we can apply our selection criteria here
#TBI have planned admissions for transplant
#brachy for homogeneity and due to physics data
#takes us to 8653 courses
rttime <-
  rttime %>%
  filter(tbi != 1 & brachy != 1) %>%
  select(-numcharge) #used billedfx to evaluate consistency; no longer need this

#check the difference between start and end
rttime$duration <- rttime$end - rttime$start

#fuse back with all the patient info
rtcourse <- left_join(rttime, patient, by = "Patient.Identifier")
rtcourse <- left_join(rtcourse, widecoursedx, by = c("Patient.Identifier", "course"))
rtcourse <- left_join(rtcourse, widecoursesubch, by = c("Patient.Identifier", "course"))
#finish here with 8653 courses
