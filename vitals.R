#vitals.R
#will pull vitals data over the past year and convert into a categorical variable

#initialize; select appropriate vitals
vitals <- select(vitals, Patient.Identifier, Date.Vitals.Taken, Diastolic.BP, Systolic.BP, Weight.in.Kilograms, 
                 Body.Mass.Index, Temperature.in.Centigrade, Pulse.in.Beats.per.Minute, Pulse.Oximetry, Level.of.Pain)

#convert the dates
vitals$Date.Vitals.Taken <- as.Date(vitals$Date.Vitals.Taken, "%m/%d/%Y %H:%M:%S")

#bring in the RT info
temp <- left_join(vitals, rttime, by="Patient.Identifier")

#define the time prior to RT the vitals were taken
temp$daysprior <- temp$start - temp$Date.Vitals.Taken

#drop out all the negative numbers (after RT start) or anything that's over 365
filtvitals <- filter(temp, daysprior < 366 & daysprior > -1)
rm(temp) #cleanup

#now recode hypothesized interesting variables with the summarize function
#sort by date
relvitals <- filtvitals[with(filtvitals, order(filtvitals$Date.Vitals.Taken)),]
relvitals <- group_by(relvitals, Patient.Identifier, course)
relvitals <- summarize(relvitals, weightloss = (max(Weight.in.Kilograms, na.rm = TRUE) - 
                     last(na.omit(Weight.in.Kilograms)))/max(Weight.in.Kilograms, na.rm = TRUE), 
                     last(na.omit(Body.Mass.Index)),
                     minsbp = min(Systolic.BP, na.rm = TRUE),
                     mindbp = min(Diastolic.BP, na.rm = TRUE),
                     maxsbp = max(Systolic.BP, na.rm = TRUE),
                     maxdbp = max(Diastolic.BP, na.rm = TRUE),
                     minpulse = min(Pulse.in.Beats.per.Minute, na.rm = TRUE),
                     maxpulse = max(Pulse.in.Beats.per.Minute, na.rm = TRUE),
                     minsat = min(Pulse.Oximetry, na.rm = TRUE), 
                     maxtemp = max(Temperature.in.Centigrade, na.rm = TRUE),
                     maxpain = max(Level.of.Pain, na.rm = TRUE)) #seems like this is doing the right thing

#now convert any inf or NaN to NA (due to NA calcs from summarize)
relvitals <- do.call(data.frame,lapply(relvitals, function(x) replace(x, is.infinite(x),NA)))
relvitals <- do.call(data.frame,lapply(relvitals, function(x) replace(x, is.nan(x),NA)))

#we need to rejoin to the rt courses now to make sure we have all the courses
temp <- left_join(select(rttime, Patient.Identifier, course), relvitals, by = c("Patient.Identifier", "course"))
rm(temp) #cleanup

abnvitals <- select(relvitals, Patient.Identifier, course, weightloss, maxpain)
abnvitals$hypo[relvitals$minsbp < 90 | relvitals$mindbp < 60] <- 1 #hypotension
abnvitals$hyper[relvitals$maxsbp > 140 | relvitals$maxdbp > 90] <- 1 #hypertension

abnvitals$tachy[relvitals$maxpulse > 100] <- 1
abnvitals$brady[relvitals$minpulse < 50] <- 1

abnvitals$hypox[relvitals$minsat < 89] <- 1

abnvitals$fever[relvitals$maxtemp >= 38] <- 1
abnvitals$pain[relvitals$maxpain > 3] <- 1

#everything will otherwise be a "no" and will be filled with 0s when final dataset is generated
