#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#meds.R
#need to consoldiate the med rec and the prescriptions and pull systemic therapy information

patientmeds <- patientmedications.csv

#Pull all the unique drug names
drugnames <- unique(patientmeds$Medication.Name)
write.csv(drugnames, file = "mednames.csv")

#go in and screen to verify no identifiers manually
#bring in the text output from RxMix
setwd("U:/TARS/source")
rxmix <- read.delim("rxmix.text", header = FALSE, sep = "|", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")

#build the dictionary to both one med name and however many classes there are
#original drug name is in the V4 column, recoded name is in V14, class is in V22
library(dplyr)
rxdict <- dplyr::select(rxmix, Medication.Name = V4, newname = V14, class = V22)

#left join the med classes with the current med list
patientmeds<- left_join(patientmeds, rxdict, by = "Medication.Name")

#fill in the unclassified drugs as other
#convert to character
patientmeds$newname <- as.character(patientmeds$newname)
patientmeds$class <- as.character(patientmeds$class)

patientmeds$newname[patientmeds$newname == ""] <- "other"
patientmeds$class[patientmeds$class == ""] <- "other"

#convert the dates
patientmeds$Start.Date <- as.Date(patientmeds$Start.Date, "%m/%d/%Y %H:%M:%S")
patientmeds$End.Date <- as.Date(patientmeds$End.Date, "%m/%d/%Y %H:%M:%S")

#build a list of antineoplastic agents so we can work on classes later
antineodict <- distinct(select(filter(patientmeds, grepl("Antineoplastic", class, fixed=TRUE)), newname)) # mesh names

#manual modifications after physician review
remove <- c("6-O-palmitoylascorbic acid", "azelaic acid", "Curcumin", "Dexamethasone", "Dexrazoxane", "Dexrazoxane hydrochloride",
            "Dihematoporphyrin Ether", "Eflornithine", "enrofloxacin", "Grape Seed Extract", "mycophenolate mofetil",
            "mycophenolate sodium", "Mycophenolic Acid", "Podofilox", "prednisolone", "Prednisone", "Resveratrol", "Sulindac")
temp <- as.data.frame(antineodict$newname[! antineodict$newname %in% remove])
rm(antineodict)
antineodict <- NULL
antineodict$newname <- temp$`antineodict$newname[!antineodict$newname %in% remove]`
rm(temp)

#need to see if they were being taken at the time of starting radiation
#rttime from the genpatient code to generate start and end dates
temp <- left_join(patientmeds, rttime, by="Patient.Identifier")

#let's make one called "recent" to reflect something like induction
#within the last 6 months but stopped taking it prior
temp$recent <- (temp$End.Date <= temp$start) & (temp$Start.Date > temp$start - 181)

#now work on "antineoplastic agents"
#let's make a separate antineoplastic agents table
antineo <- filter(temp, is.element(temp$newname, antineodict$newname))

#will call "concurrent" if there was an order that started within a month before and discontinued after start
#and reasonable to assume planned if within the first two weeks
antineo$concurrent <- (antineo$End.Date > antineo$start) & (antineo$Start.Date < antineo$start + 15) &
  (antineo$Start.Date > antineo$start - 31)

#we can make wide tables of the agent and the classes which would also simplify
agent <- distinct(antineo, Patient.Identifier, course, newname, concurrent, recent)
conagent <- filter(agent, concurrent == TRUE)
conagent$concurrent <- 1*conagent$concurrent
conagent <- select(conagent, -recent)
recagent <- filter(agent, recent == TRUE)
recagent$recent <- 1*recagent$recent
recagent <- select(recagent, -concurrent)

neoclass <- distinct(antineo, Patient.Identifier, course, class, concurrent, recent)
conclass <- filter(neoclass, concurrent == TRUE)
conclass$concurrent <- 1*conclass$concurrent
conclass <- select(conclass, -recent)
recclass <- filter(neoclass, recent == TRUE)
recclass$recent <- 1*recclass$recent
recclass <- select(recclass, -concurrent)

#now make these into wide format for each course
library(tidyr)
wideconagent <- spread(conagent, newname, concurrent)
widerecagent <- spread(recagent, newname, recent)

wideconclass <- spread(conclass, class, concurrent)
widerecclass <- spread(recclass, class, recent)

#change NAs to 0s
wideconagent[is.na(wideconagent)] <- 0
widerecagent[is.na(widerecagent)] <- 0
wideconclass[is.na(wideconclass)] <- 0
widerecclass[is.na(widerecclass)] <- 0

#finish up by adding name prefixes
names(wideconagent)[3:length(names(wideconagent))] <-
  paste0("con_", names(wideconagent)[3:length(names(wideconagent))])
names(widerecagent)[3:length(names(widerecagent))] <-
  paste0("rec_", names(widerecagent)[3:length(names(widerecagent))])
names(wideconclass)[3:length(names(wideconclass))] <-
  paste0("con_", names(wideconclass)[3:length(names(wideconclass))])
names(widerecclass)[3:length(names(widerecclass))] <-
  paste0("rec_", names(widerecclass)[3:length(names(widerecclass))])

#and let's make an any agent concurrent or recent table
anyneo <- antineo
anyneo$recent <- 1*anyneo$recent
anyneo$concurrent <- 1*anyneo$concurrent
anyneo <- group_by(anyneo, Patient.Identifier, course)
anyneo <- summarize(anyneo, concurrent = max(concurrent), recent = max(recent))

#now back to the other meds...

#now determine if the med was reportedly still being taken prior to RT
temp$taking <- 1* (temp$Start.Date <= temp$start & !(temp$End.Date <= temp$start))
temp$recent <- 1 * temp$recent

#now create a current meds and recent meds
recmeds <- filter(temp, recent == 1)
curmeds <- filter(temp, taking == 1)
rm(temp) #cleanup

##now we need to make wide formats of each. start with recmeds
temp <- select(recmeds, Patient.Identifier, course, class)
#drop the duplicates and blanks
temp$class <- as.character(temp$class)
temp$class[temp$class == ""] <- "Other"
temp <- distinct(temp)
temp$taking <- 1

#go from long to wide format
library(tidyr)
recrxmixclasses <- spread(temp, class, taking)
recrxmixclasses[is.na(recrxmixclasses)] <- 0
rm(temp) #cleanup

#quick name prefix
names(recrxmixclasses)[3:length(names(recrxmixclasses))] <-
  paste0("rec_", names(recrxmixclasses)[3:length(names(recrxmixclasses))])

##now do the current meds
temp <- select(curmeds, Patient.Identifier, course, class)
#drop the duplicates and blanks
temp$class <- as.character(temp$class)
temp$class[temp$class == ""] <- "Other"
temp <- distinct(temp)
temp$taking <- 1

#go from long to wide format
library(tidyr)
rxmixclasses <- spread(temp, class, taking)
rxmixclasses[is.na(rxmixclasses)] <- 0
rm(temp) #cleanup

names(rxmixclasses)[3:length(names(rxmixclasses))] <-
  paste0("cur_", names(rxmixclasses)[3:length(names(rxmixclasses))])
