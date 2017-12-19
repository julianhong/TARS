#create RT encounters based on departmental dates

#replace MRNs in the RT encounters with the new ones
#let's do this as a join
library(dplyr)

RTencounter <- distinct(bind_rows(RT2013.csv, RT2014.csv, RT2015.csv, RT2016.csv)) #144567 distinct

edgefx <- bind_rows(RT2012.csv, RT2017.csv) #inclusion is treatment from 2013-2016; some  may start/finish before/after
temp <- unique(RTencounter$PatientId) #pull all of our patients in the cohort
temp2 <- filter(edgefx, is.element(PatientId, temp)) #pull out the extra fractions

#the extra fractions may represent discrete treatments, but we will figure this out after we tabulate courses
RTencounter <- bind_rows(RTencounter, temp2) #should total 149614
rm(temp, temp2)

changedMrns <- distinct(bind_rows(changedMrns20132014.csv, changedMrns20152016.csv))

names(changedMrns)[1] <- "PatientId"
RTencounter <- left_join(RTencounter, changedMrns)
RTencounter$New.MRN <- as.character(RTencounter$New.MRN)
RTencounter$New.MRN[is.na(RTencounter$New.MRN)] <- RTencounter$PatientId[is.na(RTencounter$New.MRN)]
RTencounter$PatientId <- RTencounter$New.MRN
RTencounter$New.MRN <- NULL

#recode the date
RTencounter$ToDateOfService <- as.Date(RTencounter$ToDateOfService, "%m/%d/%Y %H:%M")

#bring in the patient identifier now
names(RTencounter)[1] <- "Duke.MRN"
temp <- left_join(RTencounter, select(patient, Duke.MRN, Patient.Identifier), by = "Duke.MRN") #manually review
#than those used in DEDUCE so sometimes instead of updating you actually have to backdate to an older MRN 
#to make sure things match (verified by JH)
#lets just drop the Duke MRN now that we're recoded

#one missing MRN which will be dropped
sum(is.na(temp$Patient.Identifier))
temp <- na.omit(temp)

RTencounter <- temp #now 149612
rm(temp)

#recode the treatments
#SRS-SBRT
RTencounter$srssbrt[RTencounter$ProcedureCodeDescription == "Robotic SRS -SBRT Radiation Therapy- 1st treatment" |
                              regexpr("Stereo", RTencounter$ProcedureCodeDescription) != -1 |
                              RTencounter$ProcedureCodeDescription == "Steriotactic Body / SRT cranial  course not to exceed 5 fractions"] <- 1
RTencounter$srssbrt[is.na(RTencounter$srssbrt)] <- 0
#2D3D
RTencounter$conv[regexpr("Treatment Delivery", RTencounter$ProcedureCodeDescription) != -1] <- 1
RTencounter$conv[is.na(RTencounter$conv)] <- 0
#IMRTVMAT
RTencounter$imrtvmat[RTencounter$ProcedureCodeDescription == "Intensity modulated treatm delivery, single or multiple fields" |
                              RTencounter$ProcedureCodeDescription == "IMRT- Rapid Arc"] <- 1
RTencounter$imrtvmat[is.na(RTencounter$imrtvmat)] <- 0
#Brachy, can do this for anything containing "Brachy" or "HDR"
RTencounter$brachy[regexpr("Brachy", RTencounter$ProcedureCodeDescription) != -1 |
                              regexpr("HDR", RTencounter$ProcedureCodeDescription) != -1 |
                              regexpr("Insertion", RTencounter$ProcedureCodeDescription) != -1 |
                              regexpr("Interstitial", RTencounter$ProcedureCodeDescription) != -1 |
                              regexpr("marker", RTencounter$ProcedureCodeDescription) != -1] <- 1 #seems like an insertion procedure
RTencounter$brachy[is.na(RTencounter$brachy)] <- 0
#TBI
RTencounter$tbi[RTencounter$ProcedureCodeDescription == "Total Body Photon Irradiation"] <- 1
RTencounter$tbi[is.na(RTencounter$tbi)] <- 0
#TSI
RTencounter$tsi[RTencounter$ProcedureCodeDescription == "Total Body Electron Irradiation"] <- 1
RTencounter$tsi[RTencounter$ProcedureCodeDescription == "Total body -  Electron"] <- 1
RTencounter$tsi[is.na(RTencounter$tsi)] <- 0

####code to remove all TBI cases (planned admission for transplant)
RTencounter <- filter(RTencounter, tbi != 1) #148167 after this

# #debug code to see what's been recoded
# summary(as.factor(RTencounter$ProcedureCodeDescription[is.na(RTencounter$treatmentrecode)]))

#hard part is to divide out the courses. start by sorting by date and patient
RTencounter <- arrange(RTencounter, Patient.Identifier, ToDateOfService)

#iterate through the dataframe and calculate the #days prior to the preceding fraction
#we have to initialize the first row
RTencounter$course[1] <- 1
RTencounter$daysbtwn[1] <- 1
for (i in 2:nrow(RTencounter)){
  #first, if this is a new patient it should be course 1 and days should be 1
  if (RTencounter$Patient.Identifier[i] != RTencounter$Patient.Identifier[i-1]){
    RTencounter$course[i] <- 1
    RTencounter$daysbtwn[i] <- 1
  }
  #otherwise if it is over 2 weeks later it should be course +1 and days between 1
  else if (RTencounter$ToDateOfService[i] - RTencounter$ToDateOfService[i-1] > 14){
    RTencounter$course[i] <- RTencounter$course[i-1] + 1
    RTencounter$daysbtwn[i] <- 1
  }
  #otherwise we just find the difference in days and keep the course the same
  else { 
    RTencounter$course[i] <- RTencounter$course[i-1]
    RTencounter$daysbtwn[i] <- RTencounter$ToDateOfService[i] - RTencounter$ToDateOfService[i-1]
  }
}

rm(i)

#fix diagnosis codes
#find out max diagnoses
i <- max(sapply(strsplit(as.character(RTencounter$DiagnosisId), ' ,'), length))
library(tidyr)
temp <- 
  RTencounter %>% 
  #separate it out but you will get a warning about too few
  separate(DiagnosisId, paste0("icd", 1:i), sep = " ,") %>%
  group_by(Duke.MRN, ToDateOfService) %>%
  #turn it into one column ***important used 4 columns here based on i makes row for each - 592668 rows
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
#now use summarise to pull things out
rttime <- summarise(temp, start = min(ToDateOfService), end= max(ToDateOfService),
                    attending = first(AttendingOncologistName),
                    numtx = n(), srssbrt = max(srssbrt), conv = max(conv), imrtvmat = max(imrtvmat),
                    brachy = max(brachy), tbi = max(tbi), tsi = max(tsi))
rm(temp)

#now here we'll get rid of any rogue courses that slipped in that did either finished in 2012 or started in 2017
temp <- filter(rttime, end > as.Date("2012-12-31") & start < as.Date("2017-01-01"))
rttime <- temp #9366
rm(temp)

#check the difference between start and end
rttime$duration <- rttime$end - rttime$start

#fuse back with all the patient info
rtcourse <- left_join(rttime, patient, by = "Patient.Identifier")
rtcourse <- left_join(rtcourse, widecoursedx, by = c("Patient.Identifier", "course"))
rtcourse <- left_join(rtcourse, widecoursesubch, by = c("Patient.Identifier", "course"))

#should finish with 9176 courses