#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#social.R
#pull data from the social history files

library(dplyr)

#of note, a lot of unknowns
social <- socialhistory.csv

#reformat the dates
social$Date.Current.Social.History.Reported <- as.Date(social$Date.Current.Social.History.Reported,
                                               "%m/%d/%Y %H:%M:%S")

#left join to merge with the basic course data
social <- left_join(social, rttime, by="Patient.Identifier")

#now determine if the social history we have was available prior to start time (best way to filter for known data)
social$present <- social$Date.Current.Social.History.Reported <= social$start
#now just pull the ones that were present
social <- filter(social, present == TRUE)

#pull variables of interest
social <- select(social, Patient.Identifier, course, Most.Recently.Reported.Tobacco.Use,
                 Most.Recently.Reported.Alcohol.Use, Most.Recently.Reported.Illicit.Drug.Use,
                 Currently.Sexually.Active)
social <- distinct(social)

#we should go ahead and rejoin this to the rttime so we can make NAs UNKNOWN up front
social <- left_join(select(rttime, Patient.Identifier, course), social, by = c("Patient.Identifier", "course"))

#recode the unknowns
social[social == "Defer" | social == "Not Asked" | is.na(social)] <- "UNKNOWN"
