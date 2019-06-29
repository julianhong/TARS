#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#labs.r
#process lab values, identify abnormal labs

library(dplyr)
labs <- patientlabs.csv

#now reformat the dates
labs$Specimen.Collection.Date <- as.Date(labs$Specimen.Collection.Date, "%m/%d/%Y %H:%M:%S")

#some cleanup
#some NA = sodium in the Test.Name field so fix that (conf by ref range)
labs$Test.Name[is.na(labs$Test.Name)] <- "Sodium"

#make a key with the first Test Name and every Base Name
basenames <- filter(distinct(select(labs, Test.Name, Base.Name)), Base.Name != "" & Test.Name !="")
basenames <- group_by(basenames, Base.Name)
basenames <- summarize(basenames, conv = first(Test.Name)) # dplyr version of summarize

#make a key with the first Base Name and every Test Name
testnames <- filter(distinct(select(labs, Test.Name, Base.Name)), Base.Name != "" & Test.Name !="")
testnames <- group_by(testnames, Test.Name)
testnames <- summarize(testnames, conv = first(Base.Name)) # dplyr version of summarize

#now singularize the base and test names to as best as possible eliminate redundancy
#basically what we want to do is set up a single basename that represents all test names and test descriptions
# we'll use plyr::mapvalues for this - load it and unload it after done
temp <- labs
temp$upTest.Name <- temp$Base.Name
library(plyr)
temp$upTest.Name <- mapvalues(temp$upTest.Name, from = basenames$Base.Name, to = as.character(basenames$conv))
temp$upBase.Name <- temp$Test.Name
temp$upBase.Name <- mapvalues(temp$upBase.Name, from = testnames$Test.Name, to = as.character(testnames$conv))
detach("package:plyr", unload = T)

#now let's do recoding based on the test description (has to be sequential)
testdescriptions <- filter(distinct(select(temp, Test.Description, upBase.Name)), upBase.Name != "" & Test.Description !="")
testdescriptions <- group_by(testdescriptions, Test.Description)
testdescriptions <- summarize(testdescriptions, conv = first(upBase.Name)) # dplyr version of summarize

library(plyr)
temp$upBase.Name <- mapvalues(temp$upBase.Name, from = testdescriptions$Test.Description, to = as.character(testdescriptions$conv))
detach("package:plyr", unload = T)

#need to drop the blank base names (largely rarely done labs)
recodelabs<- filter(temp, upBase.Name != "")
rm(temp)

###

#need to filter things out by date
temp <- left_join(recodelabs, rttime, by="Patient.Identifier")

#we'll call recent within 4 weeks of RT start
temp$recent <- temp$Specimen.Collection.Date <= temp$start & temp$Specimen.Collection.Date >=
  temp$start - 28

#now just pull the ones that were recent
filtlabs <- filter(temp, recent == TRUE)
rm(temp)

#start just with the set of abnormal flagged labs that are final
abnlabs <- NULL
abnlabs <- filter(filtlabs, Abnormal.Flag != "" & Status.Code == "F")

#clean up the abnormal flags
abnlabs$Abnormal.Flag <- trimws(as.character(abnlabs$Abnormal.Flag))

#making just the abnormal labs
#go from long to wide format
library(tidyr)
#drop the duplicates
wideabnlabs <- select(abnlabs, Patient.Identifier, course, upBase.Name)
wideabnlabs <- distinct(wideabnlabs)
wideabnlabs$count <- 1
wideabnlabs <- spread(wideabnlabs, upBase.Name, count)
wideabnlabs[is.na(wideabnlabs)] <- 0 #need to fill in the NAs
