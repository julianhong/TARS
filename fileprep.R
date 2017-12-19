#fileprep.R
#brings in RT files and DEDUCE files and preps them for everything else
#first thing to run

#Start with cleanup
rm(list=ls())

#adjust memory
memory.limit(size=50000)

#Import the basic patient info
setwd("U:/TARS/source")
files <- list.files(pattern = '*.csv')
for (i in 1:length(files)) {
  assign(files[i], read.csv(files[i]))
}
rm(files) #cleanup

# ###code for reformatting for DEDUCE as a prelimlinary step
#export to a csv for deduce
library(dplyr)
mrns <- bind_rows(RT2015.csv, RT2016.csv) #RT2013.csv, RT2014.csv
write.csv(unique(mrns$PatientId), file = "MRN.csv")
rm(mrns)
# #note that here there will be MRNs that were changed (we will fix that in rtencounter.r) but there is an MRN that is not in deduce
# #also had to separate things out into multiple deduce queries

#now bind everything OK to coerce factors into characters
#most of the combined files will come together in the individual scripts
library(dplyr)
patient <- distinct(bind_rows(patient20132014.csv, patient20152016.csv))
encounter <- distinct(bind_rows(encounter20132014.csv, encounter20152016.csv))
pataddressinfo <- distinct(bind_rows(pataddressinfo20132014.csv, pataddressinfo20152016.csv))
#quick reformat of the zipcodes into just the first five
pataddressinfo$Patient.Primary.Postal.Code <- substr(pataddressinfo$Patient.Primary.Postal.Code, 1,5)

#need to fix a 20+ to a number for smoking
socialhistory20152016.csv$Number.of.Years.Used.Tobacco[socialhistory20152016.csv$Number.of.Years.Used.Tobacco == 
                                                         "20+"] <- "20"
socialhistory20152016.csv$Number.of.Years.Used.Tobacco <- 
  as.numeric(socialhistory20152016.csv$Number.of.Years.Used.Tobacco)

# #cleanup is optional
# rm(patient20132014.csv, patient20152016.csv, RT2013.csv, RT2014.csv, RT2015.csv, RT2016.csv, 
#    changedMrns20132014.csv, changedMrns20152016.csv, encounter20132014.csv, encounter20152016.csv, 
#    patientlabs20132014.csv, patientlabs20152016.csv, vitals20132014.csv, vitals20152016.csv,
#    problemlists20132014.csv, problemlists20152016.csv, patientdiagnoses20132014.csv, patientdiagnoses20152016.csv,
#    patientmedications20132014.csv, patientmedications20152016.csv)

#can now run genpatient and other scripts.