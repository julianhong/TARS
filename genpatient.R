#genpatient.R
#Code to clean up the patient file (first step)

#let's clean up this table
#convert date of birth to a date
patient$Patient.Date.of.Birth <- as.Date(patient$Patient.Date.of.Birth, "%m/%d/%Y %H:%M:%S")

#demographics need to be factors too
patient$Patient.Race <- as.factor(patient$Patient.Race)
patient$Patient.Ethnic.Group <- as.factor(patient$Patient.Ethnic.Group)
patient$Patient.Marital.Status <- as.factor(patient$Patient.Marital.Status)
patient$Patient.Religion <- as.factor(patient$Patient.Religion)

#now you can run all the other scripts